-module(gen_global_view).
-include("common_data.hrl").
-export([generate/3, proc/2]).

%% API
generate(GraphMap, OutputDir, EntryPoint) ->
    MainGraph = maps:get(EntryPoint, GraphMap, no_entry_point),
    case MainGraph of
        no_entry_point ->
            no_entry_point;
        _ ->
            generate_global(GraphMap, MainGraph, EntryPoint, OutputDir)
    end.

%% Internal Functions

generate_global(List, MainGraph, Name, Dir) ->
    Gr = get_graph(List, MainGraph),
    io:fwrite("GR = ~p~n", [Gr]),
    % done = fsa:minimize(Gr),
    Data = digraph_to_dot:convert(Gr),
    common_fun:save_to_file(Data, Dir, atom_to_list(Name), global),
    done.

get_graph(GraphMap, MainGraph) ->
    RetGraph = digraph:new(),
    VNew = common_fun:add_vertex(RetGraph),
    get_graph({GraphMap, MainGraph, RetGraph}, maps:new(), 1, VNew, []).

get_graph(Data, ProcMap, VCurrent, VLast, MarkedEdges) ->
    {_GraphMap, MainGraph, RetGraph} = Data,
    OutEdges = digraph:out_edges(MainGraph, VCurrent),
    OutDegree = digraph:out_degree(MainGraph, VCurrent),
    if
        OutDegree =:= 0 ->
            % no possible transitions
            % stop other process
            stop_process(ProcMap),
            RetGraph;
        true ->
            [FirstE | T] = OutEdges,
            IsMarked = lists:member(FirstE, MarkedEdges),
            if
                not IsMarked ->
                    {NewProcMap, NewVCurrent, NewV} = main_take_transition(
                        FirstE, Data, ProcMap, VLast
                    ),

                    get_graph(Data, NewProcMap, NewVCurrent, NewV, MarkedEdges ++ [FirstE]);
                T =:= [] ->
                    % stop_process(ProcMap),
                    RetGraph;
                true ->
                    digraph:del_edge(MainGraph, FirstE),
                    get_graph(Data, ProcMap, VCurrent, VLast, MarkedEdges)
            end
    end.

stop_process(ProcMap) ->
    maps:foreach(fun(_K, V) -> V ! stop end, ProcMap).

main_take_transition(TransitionEdge, Data, ProcMap, VLast) ->
    {GraphMap, MainGraph, RetGraph} = Data,
    E = digraph:edge(MainGraph, TransitionEdge),
    {_, _, VT, Label} = E,
    SLabel = atom_to_list(Label),
    IsArg = string:find(SLabel, "arg"),
    IsSpawn = string:find(SLabel, "spawn"),
    IsSend = string:find(SLabel, "send"),
    IsReceive = string:find(SLabel, "receive"),
    if
        is_list(IsArg) ->
            io:fwrite("arg detected~n"),
            {GraphMap, VT, VLast};
        is_list(IsSpawn) ->
            io:fwrite("Spawn detected~n"),
            ProcName = string:prefix(SLabel, "spawn "),
            ProcGraph = maps:get(list_to_atom(ProcName), GraphMap),
            P = spawn(?MODULE, proc, [ProcGraph, 1]),
            NewM = maps:merge(ProcMap, #{ProcName => P}),
            VNew = common_fun:add_vertex(RetGraph),
            NewLabel = list_to_atom(ProcName ++ " spawned"),
            digraph:add_edge(RetGraph, VLast, VNew, NewLabel),
            {NewM, VT, VNew};
        is_list(IsSend) ->
            io:fwrite("send detected~n"),
            {GraphMap, VT, VLast};
        is_list(IsReceive) ->
            io:fwrite("recv detected~n"),
            {GraphMap, VT, VLast};
        true ->
            io:fwrite("other op detected: ~p~n", [SLabel]),
            {GraphMap, VT, VLast}
    end.

proc(G, VCurrent) ->
    receive
        {use_transition, E} ->
            {E, VCurrent, V, _} = digraph:edge(G, E),
            proc(G, V);
        {P, possible_transitions} ->
            P ! {self(), digraph:out_edge(G, VCurrent)},
            proc(G, VCurrent);
        stop ->
            done
    end.
