-module(global_view).
-include("../common/common_data.hrl").

%%% API
-export([generate/2, proc_loop/2]).

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
    % fsa:minimize(Gr),
    common_fun:save_graph_to_file(Gr, Dir, atol(EntryPoint), global).

create_globalview(Name) ->
    RetG = digraph:new(),
    VNew = common_fun:add_vertex(RetG),
    MainProcPid = spawn(?MODULE, proc_loop, [Name, 1]),
    ProcPidMap = #{Name => MainProcPid},
    %%% initialize data structures
    progress_proc({RetG, VNew, ProcPidMap, [], [], #{}}, 0).

progress_proc(Data, Turn) ->
    io:fwrite("Turn ~p~n", [Turn]),
    {RetG, _, ProcPidMap, _, _, _} = Data,
    {NewData, OpDone} = maps:fold(
        fun(ProcName, ProcPid, AccData) ->
            ProcOD = get_proc_out_degree(ProcPid),
            EToEval = choose_edge(ProcPid),
            if
                %%% TODO: verify this -> No out edges equals to final state?
                ProcOD =:= 0 -> AccData;
                ProcOD =/= 0 -> eval_edge(EToEval, ProcName, ProcPid, AccData)
            end
        end,
        {Data, false},
        ProcPidMap
    ),
    if
        OpDone -> progress_proc(NewData, Turn + 1);
        not OpDone -> stop_proc(ProcPidMap)
    end,
    RetG.

choose_edge(ProcPid) ->
    EdgeList = get_proc_edges(ProcPid),
    %%% pick an edge
    E = first(EdgeList),
    get_proc_edge_info(ProcPid, E).

%%% pick methods
pick_random(X) -> lists:nth(rand:uniform(length(X)), X).
first([]) -> [];
first([H | _]) -> H.

eval_edge(EdgeInfo, ProcName, ProcPid, AccData) ->
    {Data, OpDone} = AccData,
    {RetG, _, _, RecvL, SendL, StateM} = Data,
    {Edge, _, _, PLabel} = EdgeInfo,
    io:fwrite("Proc ~p eval label ~p~n", [ProcName, PLabel]),
    SLabel = atol(PLabel),
    IsEps = string:find(SLabel, "ɛ"),
    IsArg = string:find(SLabel, "arg"),
    IsSpawn = string:find(SLabel, "spawn"),
    IsSend = string:find(SLabel, "send"),
    IsReceive = string:find(SLabel, "receive"),
    if
        is_list(IsEps) ->
            ProcPid ! {use_transition, Edge},
            ProcPid ! {use_transition, Edge},
            {Data, true};
        is_list(IsArg) ->
            ProcPid ! {use_transition, Edge},
            {Data, true};
        is_list(IsSpawn) ->
            {VNew, NewM} = add_spawn_to_global(SLabel, ProcName, Data),
            ProcPid ! {use_transition, Edge},
            NewData = {RetG, VNew, NewM, RecvL, SendL, StateM},
            {NewData, true};
        is_list(IsSend) ->
            {NewData, NewOp} = manage_send(SLabel, Data, ProcName, ProcPid, EdgeInfo),
            {NewData, OpDone or NewOp};
        is_list(IsReceive) ->
            {NewData, NewOp} = manage_recv(SLabel, Data, ProcName, ProcPid, EdgeInfo),
            {NewData, OpDone or NewOp};
        true ->
            {Data, OpDone}
    end.

add_spawn_to_global(SLabel, ProcName, Data) ->
    {RetG, VLast, ProcPidMap, _, _, _} = Data,
    NewProcName = string:prefix(SLabel, "spawn "),
    NewProcPid = spawn(?MODULE, proc_loop, [ltoa(NewProcName), 1]),
    NewMap = maps:put(ltoa(NewProcName), NewProcPid, ProcPidMap),
    VNew = common_fun:add_vertex(RetG),
    %%% Δ means spawned
    NewLabel = ltoa(atol(ProcName) ++ "Δ" ++ NewProcName),
    digraph:add_edge(RetG, VLast, VNew, NewLabel),
    {VNew, NewMap}.

manage_send(SLabel, Data, ProcName, ProcPid, EdgeInfo) ->
    {Edge, _, _, _} = EdgeInfo,
    {RetG, VLast, ProcPidMap, RecvL, SendL, StateM} = Data,
    DataSent = get_data_from_label(SLabel),
    PL = [{PName, E, DataR} || {PName, E, DataR} <- RecvL, DataR =:= DataSent],
    {VNew, NewRL, NewSL, NewStateM, NewOp} =
        if
            PL =:= [] ->
                ProcPid ! {use_transition, Edge},
                AlreadyMember = lists:member({ProcName, Edge, DataSent}, SendL),
                case AlreadyMember of
                    true -> {VLast, RecvL, SendL, StateM, true};
                    false -> {VLast, RecvL, SendL ++ [{ProcName, Edge, DataSent}], StateM, true}
                end;
            PL =/= [] ->
                % pick a receiver
                Entry = pick_random(PL),
                {ProcNRecv, ProcRecvE, _} = Entry,
                PPid = maps:get(ProcNRecv, ProcPidMap),
                E2Info = get_proc_edge_info(PPid, ProcRecvE),
                NewL = ltoa(atol(ProcName) ++ "->" ++ atol(ProcNRecv) ++ ":" ++ DataSent),
                DForFunc = {StateM, RetG, VLast, NewL},
                {VNA, NewSMap} = decide_vertex(ProcName, EdgeInfo, ProcNRecv, E2Info, DForFunc),
                PPid ! {use_transition, ProcRecvE},
                ProcPid ! {use_transition, Edge},
                {VNA, lists:delete(Entry, RecvL), SendL, NewSMap, true}
        end,
    NewData = {RetG, VNew, ProcPidMap, NewRL, NewSL, NewStateM},
    {NewData, NewOp}.

manage_recv(SLabel, Data, ProcName, ProcPid, EdgeInfo) ->
    {Edge, _, _, _} = EdgeInfo,
    {RetG, VLast, ProcPidMap, RecvL, SendL, StateM} = Data,
    DataRecv = get_data_from_label(SLabel),
    PL = [{PName, E, DataS} || {PName, E, DataS} <- SendL, DataS =:= DataRecv],
    {VNew, NewRL, NewSL, NewStateM, NewOp} =
        if
            PL =:= [] ->
                AlreadyMember = lists:member({ProcName, Edge, DataRecv}, RecvL),
                case AlreadyMember of
                    true -> {VLast, RecvL, SendL, StateM, false};
                    false -> {VLast, RecvL ++ [{ProcName, Edge, DataRecv}], SendL, StateM, true}
                end;
            PL =/= [] ->
                % pick a sender
                Entry = pick_random(PL),
                {ProcSent, ProcSentE, _} = Entry,
                PPid = maps:get(ProcSent, ProcPidMap),
                E2Info = get_proc_edge_info(PPid, ProcSentE),
                NewL = ltoa(atol(ProcSent) ++ "->" ++ atol(ProcName) ++ ":" ++ DataRecv),
                DForFunc = {StateM, RetG, VLast, NewL},
                {VNA, NewSMap} = decide_vertex(ProcName, EdgeInfo, ProcSent, E2Info, DForFunc),
                ProcPid ! {use_transition, Edge},
                {VNA, RecvL, lists:delete(Entry, SendL), NewSMap, true}
        end,
    NewData = {RetG, VNew, ProcPidMap, NewRL, NewSL, NewStateM},
    {NewData, NewOp}.

decide_vertex(Proc1, Edge1, Proc2, Edge2, Data) ->
    {StateM, RetG, VLast, Label} = Data,
    {_, V1, V2, _} = Edge1,
    {_, PV1, PV2, _} = Edge2,
    Vfirst = maps:get({{Proc1, V2}, {Proc2, PV2}}, StateM, ?UNDEFINED),
    case Vfirst of
        ?UNDEFINED ->
            Vsecond = maps:get({{Proc2, PV2}, {Proc1, V2}}, StateM, ?UNDEFINED),
            case Vsecond of
                ?UNDEFINED ->
                    VAdded = common_fun:add_vertex(RetG),
                    digraph:add_edge(RetG, VLast, VAdded, Label),
                    NewM = maps:put({{Proc1, V1}, {Proc2, PV1}}, VLast, StateM),
                    {VAdded, NewM};
                _ ->
                    digraph:add_edge(RetG, VLast, Vsecond, Label),
                    {Vsecond, StateM}
            end;
        _ ->
            digraph:add_edge(RetG, VLast, Vfirst, Label),
            {Vfirst, StateM}
    end.

get_data_from_label(S) -> lists:nth(2, string:split(S, " ", all)).

% stop all the processes
stop_proc(ProcMap) -> maps:foreach(fun(_K, V) -> V ! stop end, ProcMap).

ltoa(L) -> list_to_atom(L).
atol(A) -> atom_to_list(A).

get_proc_edges(P) -> send_recv(P, {self(), get_edges}).
get_proc_current_v(P) -> send_recv(P, {self(), get_current_vertex}).
get_proc_marked(P) -> send_recv(P, {self(), get_marked}).
get_proc_out_degree(P) -> send_recv(P, {self(), get_out_degree}).
get_proc_edge_info(P, E) -> send_recv(P, {self(), get_edge_info, E}).

send_recv(P, Data) ->
    P ! Data,
    receive
        {D} -> D
    end.

proc_loop(ProcName, VCurrent) ->
    proc_loop(ProcName, VCurrent, [], []).
proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE) ->
    G = common_fun:get_fun_graph_from_db(ProcName),
    % wait(1),
    receive
        {use_transition, E} ->
            IsAlreadyMarkedOnce = lists:member(E, FirstMarkedE),
            case digraph:edge(G, E) of
                {E, VCurrent, VNew, _Label} when IsAlreadyMarkedOnce ->
                    proc_loop(ProcName, VNew, FirstMarkedE, SecondMarkedE ++ [E]);
                {E, VCurrent, VNew, _Label} ->
                    proc_loop(ProcName, VNew, FirstMarkedE ++ [E], SecondMarkedE);
                _ ->
                    io:fwrite("Edge ~p non trovato in ~p~n", [E, ProcName])
            end;
        {P, get_current_vertex} ->
            P ! {VCurrent},
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE);
        {P, get_edges} ->
            EL = digraph:out_edges(G, VCurrent),
            ERet = [E || E <- EL, not lists:member(E, SecondMarkedE)],
            P ! {ERet},
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE);
        {P, get_out_degree} ->
            P ! {digraph:out_degree(G, VCurrent)},
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE);
        {P, get_marked} ->
            P ! {FirstMarkedE, SecondMarkedE},
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE);
        {P, get_edge_info, E} ->
            P ! {digraph:edge(G, E)},
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE);
        stop ->
            ok
    end.

wait(Sec) ->
    receive
    after (Sec * 1000) ->
        ok
    end.
