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
    progress_proc({RetG, VNew, ProcPidMap, [], []}, 0).

progress_proc(Data, Turn) ->
    io:fwrite("Turn ~p~n", [Turn]),
    {RetG, _, ProcPidMap, _, _} = Data,
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
    {RetG, VLast, ProcPidMap, RecvL, SendL} = Data,
    {Edge, V1, V2, PLabel} = EdgeInfo,
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
            {Data, true};
        is_list(IsArg) ->
            ProcPid ! {use_transition, Edge},
            {Data, true};
        is_list(IsSpawn) ->
            {VNew, NewM} = add_spawn_to_global(SLabel, ProcName, Data),
            ProcPid ! {use_transition, Edge},
            NewData = {RetG, VNew, NewM, RecvL, SendL},
            {NewData, true};
        is_list(IsSend) ->
            DataSent = get_data_from_label(SLabel),
            PL = [{PName, E, DataR} || {PName, E, DataR} <- RecvL, DataR =:= DataSent],
            {VNew, NewRL, NewSL, NewOp} =
                if
                    PL =:= [] ->
                        AlreadyMember = lists:member({ProcName, Edge, DataSent}, SendL),
                        if
                            AlreadyMember ->
                                {VLast, RecvL, SendL, OpDone};
                            not AlreadyMember ->
                                {VLast, RecvL, SendL ++ [{ProcName, Edge, DataSent}], true}
                        end;
                    PL =/= [] ->
                        [Entry | _] = PL,
                        {ProcNRecv, ProcRecvE, _} = Entry,
                        VAdded = common_fun:add_vertex(RetG),
                        NewL = ltoa(atol(ProcName) ++ "->" ++ atol(ProcNRecv) ++ ":" ++ DataSent),
                        digraph:add_edge(RetG, VLast, VAdded, NewL),
                        PPid = maps:get(ProcNRecv, ProcPidMap),
                        % {_, PV1, PV2, _} = get_proc_edge_info(PPid, ProcRecvE),
                        % IsBack = V1 < V2,
                        % IsPBack = PV1 < PV2,
                        PPid ! {use_transition, ProcRecvE},
                        ProcPid ! {use_transition, Edge},
                        {VAdded, lists:delete(Entry, RecvL), SendL, true}
                end,
            NewData = {RetG, VNew, ProcPidMap, NewRL, NewSL},
            {NewData, NewOp};
        is_list(IsReceive) ->
            DataRecv = get_data_from_label(SLabel),
            PL = [{PName, E, DataS} || {PName, E, DataS} <- SendL, DataS =:= DataRecv],
            {VNew, NewRL, NewSL, NewOp} =
                if
                    PL =:= [] ->
                        AlreadyMember = lists:member({ProcName, Edge, DataRecv}, RecvL),
                        if
                            AlreadyMember ->
                                {VLast, RecvL, SendL, OpDone};
                            not AlreadyMember ->
                                {VLast, RecvL ++ [{ProcName, Edge, DataRecv}], SendL, true}
                        end;
                    PL =/= [] ->
                        [Entry | _] = PL,
                        {ProcSent, ProcSentE, _} = Entry,
                        VAdded = common_fun:add_vertex(RetG),
                        NewL = ltoa(atol(ProcSent) ++ "->" ++ atol(ProcName) ++ ":" ++ DataRecv),
                        digraph:add_edge(RetG, VLast, VAdded, NewL),
                        PPid = maps:get(ProcSent, ProcPidMap),
                        % {_, PV1, PV2, _} = get_proc_edge_info(PPid, ProcSentE),
                        % IsBack = V1 < V2,
                        % IsPBack = PV1 < PV2,
                        PPid ! {use_transition, ProcSentE},
                        ProcPid ! {use_transition, Edge},
                        {VAdded, lists:delete(Entry, RecvL), SendL, true}
                end,
            NewData = {RetG, VNew, ProcPidMap, NewRL, NewSL},
            {NewData, NewOp};
        true ->
            {Data, OpDone}
    end.

%%% Δ = spawned
add_spawn_to_global(SLabel, ProcName, Data) ->
    {RetG, VLast, ProcPidMap, _, _} = Data,
    NewProcName = string:prefix(SLabel, "spawn "),
    NewProcPid = spawn(?MODULE, proc_loop, [ltoa(NewProcName), 1]),
    NewMap = maps:put(ltoa(NewProcName), NewProcPid, ProcPidMap),
    VNew = common_fun:add_vertex(RetG),
    NewLabel = ltoa(atol(ProcName) ++ " Δ " ++ NewProcName),
    digraph:add_edge(RetG, VLast, VNew, NewLabel),
    {VNew, NewMap}.

get_data_from_label(S) ->
    lists:nth(2, string:split(S, " ", all)).

% stop other processes
stop_proc(ProcMap) ->
    maps:foreach(fun(_K, V) -> V ! stop end, ProcMap).

ltoa(L) -> list_to_atom(L).
atol(A) -> atom_to_list(A).

get_proc_edges(P) ->
    P ! {self(), get_edges},
    recv().

get_proc_current_v(P) ->
    P ! {self(), get_current_vertex},
    recv().

get_proc_marked(P) ->
    P ! {self(), get_marked},
    recv().

get_proc_out_degree(P) ->
    P ! {self(), get_out_degree},
    recv().

get_proc_edge_info(P, E) ->
    P ! {self(), get_edge_info, E},
    recv().

recv() ->
    receive
        {D} -> D
    end.

proc_loop(ProcName, VCurrent) ->
    proc_loop(ProcName, VCurrent, []).
proc_loop(ProcName, VCurrent, MarkedEdge) ->
    G = common_fun:get_fun_graph_from_db(ProcName),
    % wait(1),
    receive
        {use_transition, E} ->
            Cond = not lists:member(E, MarkedEdge),
            case digraph:edge(G, E) of
                {E, VCurrent, VNew, _Label} when Cond ->
                    proc_loop(ProcName, VNew, MarkedEdge ++ [E]);
                false ->
                    io:fwrite("Edge ~p non trovato in ~p~n", [E, ProcName])
            end;
        {P, get_current_vertex} ->
            P ! {VCurrent},
            proc_loop(ProcName, VCurrent, MarkedEdge);
        {P, get_edges} ->
            EL = digraph:out_edges(G, VCurrent),
            ERet = [E || E <- EL, not lists:member(E, MarkedEdge)],
            P ! {ERet},
            proc_loop(ProcName, VCurrent, MarkedEdge);
        {P, get_out_degree} ->
            P ! {digraph:out_degree(G, VCurrent)},
            proc_loop(ProcName, VCurrent, MarkedEdge);
        {P, get_marked} ->
            P ! {MarkedEdge},
            proc_loop(ProcName, VCurrent, MarkedEdge);
        {P, get_edge_info, E} ->
            P ! {digraph:edge(G, E)},
            proc_loop(ProcName, VCurrent, MarkedEdge);
        stop ->
            ok
    end.

wait(Sec) ->
    receive
    after (Sec * 1000) ->
        ok
    end.
