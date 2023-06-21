-module(global_view).
-include("../common/common_data.hrl").

%%% API
-export([generate/2, proc_loop/2]).

-record(branch, {graph, last_vertex, proc_pid_m, recv_l, send_l, states_m}).

%%%===================================================================
%%% API
%%%===================================================================

generate(OutputDir, EntryPoint) ->
    MainGraph = common_fun:get_fun_graph_from_db(EntryPoint),
    case MainGraph of
        no_graph_found -> no_entry_point_found;
        _ -> generate_global(EntryPoint, OutputDir)
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

generate_global(EntryPoint, Dir) ->
    Gr = create_globalview(EntryPoint),
    MG = fsa:minimize(Gr),
    %%% buffo doppio minimize a caso
    MG1 = fsa:minimize(MG),
    common_fun:save_graph_to_file(MG1, Dir, atol(EntryPoint), global).

create_globalview(Name) ->
    RetG = digraph:new(),
    VNew = common_fun:add_vertex(RetG),
    MainProcPid = spawn(?MODULE, proc_loop, [Name, 1]),
    ProcPidMap = #{Name => MainProcPid},
    %%% initialize data structures
    progress_proc(RetG, [new_branch(RetG, VNew, ProcPidMap, [], [], #{})], 0).

new_branch(G, V, P, R, S, M) ->
    #branch{
        graph = G,
        last_vertex = V,
        proc_pid_m = P,
        recv_l = R,
        send_l = S,
        states_m = M
    }.

progress_proc(G, [], _) ->
    G;
progress_proc(GlobalGraph, BranchList, Turn) when is_list(BranchList) ->
    io:fwrite("~nTurn ~p~n", [Turn]),
    io:fwrite("Branch to eval ~p~n", [length(BranchList)]),
    NewBL = lists:foldl(
        fun(Item, AccL) ->
            io:fwrite("Eval branch~n"),
            NewBreanches = progress_single_branch(Item),
            AccL ++ NewBreanches
        end,
        [],
        BranchList
    ),
    progress_proc(GlobalGraph, NewBL, Turn + 1).

progress_single_branch(BData) ->
    progress_single_branch(BData, []).

progress_single_branch(BData, AccL) ->
    {NewBData, NBL, Op1} = maps:fold(
        fun(Name, Pid, AccData) -> eval_proc_until_send(Name, Pid, AccData) end,
        {BData, [], false},
        BData#branch.proc_pid_m
    ),
    SendList = maps:fold(
        fun(Name, Pid, AL) -> AL ++ get_possible_send(Name, Pid) end,
        [],
        NewBData#branch.proc_pid_m
    ),
    case SendList =:= [] of
        true ->
            case Op1 of
                true -> progress_single_branch(NewBData, AccL ++ NBL);
                false -> AccL
            end;
        false ->
            io:fwrite("SendList ~p~n", [SendList]),
            {NL, Modified} = lists:foldl(
                fun(I, A) ->
                    io:fwrite("Eval ~p~n", [I]),
                    {L, O} = A,
                    {PName, SLabel, E} = I,
                    {ND, Op} = manage_send(SLabel, dup_branch(NewBData), PName, E),
                    {L ++ [ND], O or Op}
                end,
                {[], false},
                SendList
            ),
            stop_processes(NewBData#branch.proc_pid_m),
            case (NL =:= []) and (not Modified) of
                true ->
                    AccL;
                false ->
                    [H | T] = NL,
                    io:fwrite("~n"),
                    progress_single_branch(H, AccL ++ NBL ++ T)
            end
    end.

dup_branch(Data) ->
    Data#branch{proc_pid_m = duplicate_proccess(Data#branch.proc_pid_m)}.

duplicate_proccess(ProcMap) ->
    maps:fold(
        fun(K, V, A) ->
            NewPid = spawn(?MODULE, proc_loop, [K, 1]),
            set_proc_data(NewPid, get_proc_data(V)),
            maps:put(K, NewPid, A)
        end,
        #{},
        ProcMap
    ).

eval_proc_until_send(ProcName, ProcPid, AccData) ->
    ProcOD = get_proc_out_degree(ProcPid),
    {Data, NBL, Bool} = AccData,
    EL = get_proc_edges(ProcPid),
    {NewData, NL, OpDone} =
        if
            %%% TODO: verify this -> No out edges equals to final state?
            ProcOD =:= 0 ->
                {Data, [], false};
            ProcOD =:= 1 ->
                EToEval = choose_edge(ProcPid),
                {D, O} = eval_edge(EToEval, ProcName, ProcPid, Data),
                {D, [], O};
            true ->
                Cond = is_lists_edgerecv(ProcPid, EL),
                case Cond of
                    true ->
                        {DataRet, _} = lists:foldl(
                            fun(E, A) ->
                                {DataRecv, MatchFound} = A,
                                case MatchFound of
                                    true ->
                                        A;
                                    false ->
                                        {B, _, C} = DataRecv,
                                        EInfo = get_proc_edge_info(ProcPid, E),
                                        {D, O, NewMatch} = manage_recv(B, ProcName, ProcPid, EInfo),
                                        {{D, [], C or O}, NewMatch}
                                end
                            end,
                            {{Data, [], false}, false},
                            EL
                        ),
                        DataRet;
                    %%% TODO da testare, non ci entra mai il programma qua
                    false ->
                        {NewL, NOp} = lists:foldl(
                            fun(E, A) ->
                                EInfo = get_proc_edge_info(ProcPid, E),
                                {B, C} = A,
                                {D, O} = eval_edge(EInfo, ProcName, ProcPid, Data),
                                {B ++ [D], C or O}
                            end,
                            {[], false},
                            EL
                        ),
                        [H | T] = NewL,
                        {H, T, NOp}
                end
        end,
    case OpDone of
        false -> {NewData, NBL, Bool};
        true -> eval_proc_until_send(ProcName, ProcPid, {NewData, NBL ++ NL, true})
    end.

is_lists_edgerecv(ProcPid, EL) ->
    lists:foldl(
        fun(E, A) ->
            {E, _, _, Label} = get_proc_edge_info(ProcPid, E),
            SLabel = atol(Label),
            IsRecv = string:find(SLabel, "receive"),
            A and is_list(IsRecv)
        end,
        true,
        EL
    ).

get_possible_send(ProcName, ProcPid) ->
    ProcOD = get_proc_out_degree(ProcPid),
    if
        ProcOD =:= 0 ->
            [];
        true ->
            EL = get_proc_edges(ProcPid),
            io:fwrite("EdgeList ~p for ~p~n", [EL, ProcName]),
            lists:foldl(
                fun(E, A) ->
                    EInfo = get_proc_edge_info(ProcPid, E),
                    {_, _, _, PLabel} = EInfo,
                    SLabel = atol(PLabel),
                    IsSend = string:find(SLabel, "send"),
                    A ++
                        case is_list(IsSend) of
                            true -> [{ProcName, SLabel, EInfo}];
                            false -> []
                        end
                end,
                [],
                EL
            )
    end.

choose_edge(ProcPid) ->
    EL = get_proc_edges(ProcPid),
    %%% pick an edge
    E = common_fun:first(EL),
    get_proc_edge_info(ProcPid, E).

eval_edge(EdgeInfo, ProcName, ProcPid, BData) ->
    {Edge, _, _, PLabel} = EdgeInfo,
    io:fwrite("Proc ~p eval label ~p~n", [ProcName, PLabel]),
    SLabel = atol(PLabel),
    IsArg = string:find(SLabel, "arg"),
    IsSpawn = string:find(SLabel, "spawn"),
    IsReceive = string:find(SLabel, "receive"),
    if
        is_list(IsArg) ->
            ProcPid ! {use_transition, Edge},
            {BData, true};
        is_list(IsSpawn) ->
            {VNew, _NewProcName, _NewProcPid, NewM} = add_spawn_to_global(SLabel, ProcName, BData),
            ProcPid ! {use_transition, Edge},
            NewBData = BData#branch{last_vertex = VNew, proc_pid_m = NewM},
            {NewBData, true};
        is_list(IsReceive) ->
            manage_recv(BData, ProcName, ProcPid, EdgeInfo);
        true ->
            {BData, false}
    end.

add_spawn_to_global(SLabel, ProcName, Data) ->
    NewProcName = string:prefix(SLabel, "spawn "),
    NewProcPid = spawn(?MODULE, proc_loop, [ltoa(NewProcName), 1]),
    NewMap = maps:put(ltoa(NewProcName), NewProcPid, Data#branch.proc_pid_m),
    VNew = common_fun:add_vertex(Data#branch.graph),
    %%% Δ means spawned
    NewLabel = ltoa(atol(ProcName) ++ "Δ" ++ NewProcName),
    digraph:add_edge(Data#branch.graph, Data#branch.last_vertex, VNew, NewLabel),
    {VNew, NewProcName, NewProcPid, NewMap}.

manage_send(SLabel, Data, ProcName, EdgeInfo) ->
    {Edge, _, _, _} = EdgeInfo,
    ProcPid = maps:get(ProcName, Data#branch.proc_pid_m),
    ProcPid ! {use_transition, Edge},
    DataSent = get_data_from_label(SLabel),
    ProcSent = ltoa(get_proc_from_label(SLabel)),
    SendL = Data#branch.send_l,
    RecvL = Data#branch.recv_l,
    io:fwrite("Send "),
    PL = find_compatibility(RecvL, ProcSent, DataSent),
    if
        PL =:= [] ->
            AlreadyMember = lists:member({ProcName, Edge, ProcSent, DataSent}, SendL),
            case AlreadyMember of
                true ->
                    {Data, false};
                false ->
                    {Data#branch{send_l = SendL ++ [{ProcName, Edge, ProcSent, DataSent}]}, true}
            end;
        PL =/= [] ->
            % pick a receiver
            Entry = common_fun:first(PL),
            {ProcNRecv, ProcRecvE, _, _} = Entry,
            PPid = maps:get(ProcNRecv, Data#branch.proc_pid_m),
            E2Info = get_proc_edge_info(PPid, ProcRecvE),
            NewL = ltoa(atol(ProcName) ++ "→" ++ atol(ProcNRecv) ++ ":" ++ DataSent),
            {VNA, NewSMap} = decide_vertex(ProcName, EdgeInfo, ProcNRecv, E2Info, Data, NewL),
            PPid ! {use_transition, ProcRecvE},
            NewRecvL = delete_recv(lists:delete(Entry, RecvL), ProcNRecv),
            {Data#branch{last_vertex = VNA, recv_l = NewRecvL, states_m = NewSMap}, true}
    end.

manage_recv(Data, ProcName, ProcPid, EdgeInfo) ->
    {Edge, _, _, PLabel} = EdgeInfo,
    SLabel = atol(PLabel),
    DataRecv = get_data_from_label(SLabel),
    RecvL = Data#branch.recv_l,
    SendL = Data#branch.send_l,
    io:fwrite("Recv "),
    CompatibleRv = find_compatibility(SendL, ProcName, DataRecv),
    case CompatibleRv =:= [] of
        true ->
            AlreadyMember = lists:member({ProcName, Edge, ProcName, DataRecv}, RecvL),
            case AlreadyMember of
                true ->
                    {Data, false, false};
                false ->
                    {
                        Data#branch{recv_l = RecvL ++ [{ProcName, Edge, ProcName, DataRecv}]},
                        true,
                        false
                    }
            end;
        false ->
            % pick a sender
            E = common_fun:first(CompatibleRv),
            io:fwrite("E chose ~p~n", [E]),
            {ProcSent, ProcSentE, _, _} = E,
            PPid = maps:get(ProcSent, Data#branch.proc_pid_m),
            E2Info = get_proc_edge_info(PPid, ProcSentE),
            NewL = ltoa(atol(ProcSent) ++ "→" ++ atol(ProcName) ++ ":" ++ DataRecv),
            {VNA, NewSM} = decide_vertex(ProcName, EdgeInfo, ProcSent, E2Info, Data, NewL),
            NewRecvL = delete_recv(RecvL, ProcName),
            ProcPid ! {use_transition, Edge},
            {
                Data#branch{
                    last_vertex = VNA,
                    recv_l = NewRecvL,
                    send_l = lists:delete(E, SendL),
                    states_m = NewSM
                },
                true,
                true
            }
    end.

delete_recv(RecvL, ProcName) ->
    lists:filter(
        fun(El) ->
            {Name, _, _, _} = El,
            ProcName =/= Name
        end,
        RecvL
    ).

find_compatibility(List, Name, Message) ->
    io:fwrite("Find in ~p for ~p and ~p~n", [List, Name, Message]),
    [
        {PName, E, ProcSent, Data}
     || {PName, E, ProcSent, Data} <- List, ProcSent =:= Name, Data =:= Message
    ].

decide_vertex(Proc1, Edge1, Proc2, Edge2, Data, Label) ->
    StateM = Data#branch.states_m,
    VLast = Data#branch.last_vertex,
    G = Data#branch.graph,
    {_, V1, V2, _} = Edge1,
    {_, PV1, PV2, _} = Edge2,
    Vfirst = maps:get({{Proc1, V2}, {Proc2, PV2}}, StateM, ?UNDEFINED),
    case Vfirst of
        ?UNDEFINED ->
            Vsecond = maps:get({{Proc2, PV2}, {Proc1, V2}}, StateM, ?UNDEFINED),
            case Vsecond of
                ?UNDEFINED ->
                    EL = digraph:out_edges(G, VLast),
                    {AlreadyExist, VCase} = lists:foldl(
                        fun(E, A) ->
                            {E, VLast, VTo, VLabel} = digraph:edge(G, E),
                            case VLabel =:= Label of
                                true -> {true, VTo};
                                false -> A
                            end
                        end,
                        {false, VLast},
                        EL
                    ),
                    case AlreadyExist of
                        true ->
                            {VCase, StateM};
                        false ->
                            VAdded = common_fun:add_vertex(G),
                            digraph:add_edge(G, VLast, VAdded, Label),
                            NewM = maps:put({{Proc1, V1}, {Proc2, PV1}}, VLast, StateM),
                            % io:fwrite("Created new node from ~p to ~p with label ~p~n", [VLast, VAdded, Label]),
                            {VAdded, NewM}
                    end;
                _ ->
                    digraph:add_edge(G, VLast, Vsecond, Label),
                    % io:fwrite("Created new edge from ~p to ~p with label ~p~n", [VLast, Vsecond, Label]),
                    {Vsecond, StateM}
            end;
        _ ->
            digraph:add_edge(G, VLast, Vfirst, Label),
            % io:fwrite("Created new edge from ~p to ~p with label ~p~n", [VLast, Vfirst, Label]),
            {Vfirst, StateM}
    end.

get_data_from_label(S) -> lists:nth(2, string:split(S, " ", all)).
get_proc_from_label(S) -> lists:nth(4, string:split(S, " ", all)).

% stop all the processes
stop_processes(ProcMap) -> maps:foreach(fun(_K, V) -> V ! stop end, ProcMap).

ltoa(L) -> list_to_atom(L).
atol(A) -> atom_to_list(A).

get_proc_edges(P) -> send_recv(P, {self(), get_edges}).
get_proc_out_degree(P) -> send_recv(P, {self(), get_out_degree}).
get_proc_edge_info(P, E) -> send_recv(P, {self(), get_edge_info, E}).
get_proc_data(P) -> send_recv(P, {self(), get_data}).
set_proc_data(P, Data) -> P ! {set_data, Data}.

send_recv(P, Data) ->
    P ! Data,
    receive
        {D} -> D
    end.

proc_loop(ProcName, VCurrent) ->
    proc_loop(ProcName, VCurrent, [], []).
proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE) ->
    G = common_fun:get_fun_graph_from_db(ProcName),
    % timer:sleep(200),
    receive
        {use_transition, E} ->
            IsAlreadyMarkedOnce = lists:member(E, FirstMarkedE),
            case digraph:edge(G, E) of
                {E, VCurrent, VNew, _} when IsAlreadyMarkedOnce ->
                    proc_loop(ProcName, VNew, FirstMarkedE, SecondMarkedE ++ [E]);
                {E, VCurrent, VNew, _} ->
                    proc_loop(ProcName, VNew, FirstMarkedE ++ [E], SecondMarkedE);
                _ ->
                    io:fwrite("V ~p edge ~p non trovato in ~p~n", [VCurrent, E, ProcName]),
                    proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE)
            end;
        {P, get_edges} ->
            EL = digraph:out_edges(G, VCurrent),
            ERet = filter_marked_edges(EL, SecondMarkedE),
            P ! {ERet},
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE);
        {P, get_out_degree} ->
            P ! {digraph:out_degree(G, VCurrent)},
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE);
        {P, get_edge_info, E} ->
            P ! {digraph:edge(G, E)},
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE);
        {P, get_data} ->
            P ! {{VCurrent, FirstMarkedE, SecondMarkedE}},
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE);
        {set_data, {V, FE, SE}} ->
            proc_loop(ProcName, V, FE, SE);
        stop ->
            ok
    end.

filter_marked_edges(EdgeL, MarkedE) -> [E || E <- EdgeL, not lists:member(E, MarkedE)].
