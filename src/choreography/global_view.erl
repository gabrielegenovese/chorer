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
    % MG = fsa:minimize(Gr),
    common_fun:save_graph_to_file(Gr, Dir, atol(EntryPoint), global).

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
    io:fwrite("Turn ~p~n", [Turn]),
    NewBL = lists:foldl(
        fun(Item, AccL) ->
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
    TempSendList = maps:fold(
        fun(Name, Pid, AL) -> AL ++ get_possible_send(Name, Pid) end,
        [],
        BData#branch.proc_pid_m
    ),
    SendList = remove_already_eval_send(BData, TempSendList),
    case SendList =:= [] of
        true ->
            case Op1 of
                true -> progress_single_branch(NewBData, AccL ++ NBL);
                false -> AccL
            end;
        false ->
            {NL, Modified} = lists:foldl(
                fun(I, A) ->
                    {L, O} = A,
                    {PName, SLabel, PPid, E} = I,
                    {ND, Op} = manage_send(SLabel, dup_branch(NewBData), PName, PPid, E),
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

remove_already_eval_send(Data, SendL) ->
    DSendL = Data#branch.send_l,
    lists:filter(
        fun({ProcName, SLabel, _, EInfo}) ->
            lists:foldl(
                fun({PName, E, Message}, A) ->
                    {Edge, _, _, _} = EInfo,
                    A and (not (ProcName =:= PName) and (SLabel =:= Message) and (Edge =:= E))
                end,
                true,
                DSendL
            )
        end,
        SendL
    ).

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
                        lists:foldl(
                            fun(E, A) ->
                                EInfo = get_proc_edge_info(ProcPid, E),
                                {B, _, C} = A,
                                {D, O} = eval_edge(EInfo, ProcName, ProcPid, B),
                                {D, [], C or O}
                            end,
                            {Data, [], false},
                            EL
                        );
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
            lists:foldl(
                fun(E, A) ->
                    EInfo = get_proc_edge_info(ProcPid, E),
                    {_, _, _, PLabel} = EInfo,
                    SLabel = atol(PLabel),
                    IsSend = string:find(SLabel, "send"),
                    A ++
                        case is_list(IsSend) of
                            true -> [{ProcName, SLabel, ProcPid, EInfo}];
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
    E = common_fun:pick_random(EL),
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
            manage_recv(SLabel, BData, ProcName, ProcPid, EdgeInfo);
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

manage_send(SLabel, Data, ProcName, ProcPid, EdgeInfo) ->
    {Edge, _, _, _} = EdgeInfo,
    DataSent = get_data_from_label(SLabel),
    PL = find_compatibility(Data#branch.recv_l, DataSent),
    if
        PL =:= [] ->
            ProcPid ! {use_transition, Edge},
            AlreadyMember = lists:member({ProcName, Edge, DataSent}, Data#branch.send_l),
            case AlreadyMember of
                true ->
                    {Data, false};
                false ->
                    {Data#branch{send_l = Data#branch.send_l ++ [{ProcName, Edge, DataSent}]}, true}
            end;
        PL =/= [] ->
            % pick a receiver
            Entry = common_fun:pick_random(PL),
            {ProcNRecv, ProcRecvE, _} = Entry,
            PPid = maps:get(ProcNRecv, Data#branch.proc_pid_m),
            E2Info = get_proc_edge_info(PPid, ProcRecvE),
            NewL = ltoa(atol(ProcName) ++ "→" ++ atol(ProcNRecv) ++ ":" ++ DataSent),
            {VNA, NewSMap} = decide_vertex(ProcName, EdgeInfo, ProcNRecv, E2Info, Data, NewL),
            PPid ! {use_transition, ProcRecvE},
            ProcPid ! {use_transition, Edge},
            {
                Data#branch{
                    last_vertex = VNA,
                    recv_l = lists:delete(Entry, Data#branch.recv_l),
                    states_m = NewSMap
                },
                true
            }
    end.

manage_recv(SLabel, Data, ProcName, ProcPid, EdgeInfo) ->
    {Edge, _, _, _} = EdgeInfo,
    DataRecv = get_data_from_label(SLabel),
    RecvL = Data#branch.recv_l,
    SendL = Data#branch.send_l,
    CompatibleRv = find_compatibility(SendL, DataRecv),
    case CompatibleRv =:= [] of
        true ->
            AlreadyMember = lists:member({ProcName, Edge, DataRecv}, RecvL),
            case AlreadyMember of
                true -> {Data, false};
                false -> {Data#branch{recv_l = RecvL ++ [{ProcName, Edge, DataRecv}]}, true}
            end;
        false ->
            % pick a sender
            E = common_fun:pick_random(CompatibleRv),
            {ProcSent, ProcSentE, _} = E,
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
                true
            }
    end.

delete_recv(RecvL, ProcName) ->
    lists:filter(
        fun(El) ->
            {Name, _, _} = El,
            ProcName =/= Name
        end,
        RecvL
    ).

find_compatibility(List, Message) ->
    [{PName, E, Data} || {PName, E, Data} <- List, Data =:= Message].

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
                    VAdded = common_fun:add_vertex(G),
                    digraph:add_edge(G, VLast, VAdded, Label),
                    NewM = maps:put({{Proc1, V1}, {Proc2, PV1}}, VLast, StateM),
                    {VAdded, NewM};
                _ ->
                    digraph:add_edge(G, VLast, Vsecond, Label),
                    {Vsecond, StateM}
            end;
        _ ->
            digraph:add_edge(G, VLast, Vfirst, Label),
            {Vfirst, StateM}
    end.

get_data_from_label(S) -> lists:nth(2, string:split(S, " ", all)).

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
            ERet = [E || E <- EL, not lists:member(E, SecondMarkedE)],
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
