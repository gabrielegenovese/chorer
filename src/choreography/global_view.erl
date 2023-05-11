-module(global_view).
-include("../common/common_data.hrl").
-export([generate/2, proc/2]).

%% API
generate(OutputDir, EntryPoint) ->
    MainGraph = common_fun:get_fun_graph_from_db(EntryPoint),
    case MainGraph of
        no_entry_point ->
            no_entry_point;
        _ ->
            generate_global(MainGraph, EntryPoint, OutputDir)
    end.

%% Internal Functions

generate_global(MainGraph, Name, Dir) ->
    Gr = get_graph(MainGraph),
    % fsa:minimize(Gr),
    Data = digraph_to_dot:convert(Gr),
    common_fun:save_to_file(Data, Dir, atom_to_list(Name), global),
    done.

get_graph(MainGraph) ->
    RetGraph = digraph:new(),
    VNew = common_fun:add_vertex(RetGraph),
    get_graph({MainGraph, RetGraph}, maps:new(), 1, VNew, []).

get_graph(Data, ProcMap, VCurrent, VLast, MarkedE) ->
    {MainGraph, RetGraph} = Data,
    OutDegree = digraph:out_degree(MainGraph, VCurrent),
    if
        % no possible transitions
        OutDegree =:= 0 ->
            stop_proc(ProcMap),
            RetGraph;
        true ->
            % take random edge
            [E | _] = digraph:out_edges(MainGraph, VCurrent),
            IsMarked = lists:member(E, MarkedE),
            if
                not IsMarked ->
                    {NewMap, NewCurr, NewV} = main_take_transition(E, Data, ProcMap, VLast),
                    get_graph(Data, NewMap, NewCurr, NewV, MarkedE ++ [E]);
                true ->
                    digraph:del_edge(MainGraph, E),
                    get_graph(Data, ProcMap, VCurrent, VLast, MarkedE)
            end
    end.

% stop other processes
stop_proc(ProcMap) ->
    maps:foreach(fun(_K, V) -> V ! stop end, ProcMap).

main_take_transition(TransitionEdge, FuncData, ProcMap, VLast) ->
    {MainGraph, RetGraph} = FuncData,
    E = digraph:edge(MainGraph, TransitionEdge),
    {_, _, VT, Label} = E,
    io:fwrite("This label: ~p~n", [Label]),
    SLabel = atom_to_list(Label),
    IsArg = string:find(SLabel, "arg"),
    IsSpawn = string:find(SLabel, "spawn"),
    IsSend = string:find(SLabel, "send"),
    IsReceive = string:find(SLabel, "receive"),
    if
        is_list(IsArg) ->
            {ProcMap, VT, VLast};
        is_list(IsSpawn) ->
            ProcName = string:prefix(SLabel, "spawn "),
            ProcGraph = common_fun:get_fun_graph_from_db(list_to_atom(ProcName)),
            P = spawn(?MODULE, proc, [ProcGraph, 1]),
            % todo cambia
            [FirstEE | _] = get_proc_edges(P),
            P ! {use_transition, FirstEE},
            % end
            NewM = maps:merge(ProcMap, #{ProcName => P}),
            VNew = common_fun:add_vertex(RetGraph),
            NewLabel = list_to_atom(ProcName ++ " spawned"),
            digraph:add_edge(RetGraph, VLast, VNew, NewLabel),
            {NewM, VT, VNew};
        is_list(IsSend) ->
            DataSent = lists:nth(2, string:split(SLabel, " ", all)),
            VNew = maps:fold(
                fun(K, V, Acc) ->
                    OE = get_proc_edges(V),
                    Found = find_receive_data(DataSent, OE, V),
                    case Found of
                        none ->
                            Acc;
                        ok ->
                            VNew = common_fun:add_vertex(RetGraph),
                            NewLabel = list_to_atom("main " ++ K ++ " ! " ++ DataSent),
                            digraph:add_edge(RetGraph, VLast, VNew, NewLabel),
                            VNew
                    end
                end,
                VLast,
                ProcMap
            ),
            {ProcMap, VT, VNew};
        is_list(IsReceive) ->
            DataRecv = lists:nth(2, string:split(SLabel, " ", all)),
            VNew = maps:fold(
                fun(K, V, _Acc) ->
                    OE = get_proc_edges(V),
                    Found = find_send_data(DataRecv, OE, V),
                    case Found of
                        none ->
                            VNew = common_fun:add_vertex(RetGraph),
                            NewLabel = list_to_atom("user main ! " ++ DataRecv),
                            digraph:add_edge(RetGraph, VLast, VNew, NewLabel),
                            VNew;
                        ok ->
                            VNew = common_fun:add_vertex(RetGraph),
                            NewLabel = list_to_atom(K ++ " main ! " ++ DataRecv),
                            digraph:add_edge(RetGraph, VLast, VNew, NewLabel),
                            VNew
                    end
                end,
                VLast,
                ProcMap
            ),
            {ProcMap, VT, VNew};
        true ->
            io:fwrite("other op detected: ~p~n", [SLabel]),
            {ProcMap, VT, VLast}
    end.

get_proc_edges(P) ->
    P ! {self(), get_transitions},
    recv().

get_proc_edge_info(P, E) ->
    P ! {self(), get_edge_info, E},
    recv().

recv() ->
    receive
        {D} -> D
    end.

find_receive_data(Data, E, P) ->
    case for_each_f(E, "receive", Data, P) of
        false ->
            none;
        Ed ->
            P ! {use_transition, Ed},
            ok
    end.

find_send_data(Data, E, P) ->
    case for_each_f(E, "send", Data, P) of
        false ->
            none;
        Ed ->
            P ! {use_transition, Ed},
            ok
    end.

for_each_f(E, Op, Data, P) ->
    lists:foldl(
        fun(Ev, Acc) ->
            {_, _V1, _V2, Label} = get_proc_edge_info(P, Ev),
            SLabel = atom_to_list(Label),
            IsOp = is_list(string:find(SLabel, Op)),
            if
                IsOp ->
                    IsData = is_list(string:find(SLabel, Data)),
                    if
                        IsData ->
                            Ev;
                        true ->
                            Acc
                    end;
                true ->
                    Acc
            end
        end,
        false,
        E
    ).

proc(G, VCurrent) ->
    receive
        {use_transition, E} ->
            {E, VCurrent, VNew, _Label} = digraph:edge(G, E),
            proc(G, VNew);
        {P, get_transitions} ->
            P ! {digraph:out_edges(G, VCurrent)},
            proc(G, VCurrent);
        {P, get_edge_info, E} ->
            P ! {digraph:edge(G, E)},
            proc(G, VCurrent);
        stop ->
            done
    end.
