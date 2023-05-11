-module(local_view).
-include("../common/common_data.hrl").

%%% API
-export([generate/1]).

generate(OutputDir) ->
    ActorList = common_fun:get_actors_from_db(),
    lists:foreach(
        fun(ActorName) ->
            ActorAst = common_fun:get_fun_ast_from_db(ActorName),
            create_localview(ActorName, ActorAst, OutputDir)
        end,
        ActorList
    ).

%%% Internal Functions

create_localview(ActorName, ActorAst, OutputDir) ->
    Gr = get_graph(ActorName, ActorAst, true),
    fsa:minimize(Gr),
    ?DBMANAGER ! {set_fun_graph, ActorName, Gr},
    FileName = atom_to_list(ActorName),
    common_fun:save_graph_to_file(Gr, OutputDir, FileName, local).

get_graph(FunName, Code, SetAdditionalInfo) when is_list(Code) ->
    Gr = digraph:new(),
    VStart = common_fun:add_vertex(Gr),
    lists:foreach(
        fun(Line) ->
            case Line of
                {clause, _, Vars, Guard, Content} ->
                    if
                        SetAdditionalInfo ->
                            VN = add_args_to_graph(Gr, Vars, Guard, VStart, false),
                            VFinal = eval_pm_clause(Content, FunName, Gr, VN),
                            set_as_final(Gr, VFinal);
                        true ->
                            eval_pm_clause(Content, FunName, Gr, VStart)
                    end;
                _ ->
                    ?UNDEFINED
            end
        end,
        Code
    ),
    Gr.

add_args_to_graph(Gr, Vars, Guard, VStart, Bool) ->
    if
        Bool ->
            VN = common_fun:add_vertex(Gr),
            EdLabel = format_label_pm_edge(Vars, Guard, "arg "),
            digraph:add_edge(Gr, VStart, VN, list_to_atom(EdLabel)),
            VN;
        true ->
            VStart
    end.

eval_pm_clause(Code, FunName, Gr, VStart) ->
    lists:foldl(
        fun(Line, VLast) ->
            eval_codeline(Line, FunName, Gr, VLast)
        end,
        VStart,
        Code
    ).

eval_codeline(CodeLine, FunName, G, VLast) ->
    case CodeLine of
        %%% In a match code line (Var = ...), we just evaluate the left
        %%% content because it could be a receive, case, if, etc...
        %%% Future TODO: in the right content there could be a variable,
        %%% so we can save it and its content type (pid, atom, list, etc...) on the dbmanager
        {match, _, _RightContent, LeftContent} ->
            eval_codeline(LeftContent, FunName, G, VLast);
        %%% If there's a recursive call, just add an epsilon edge to the first state
        {call, _, {atom, _, FunName}, _} ->
            digraph:add_edge(G, VLast, 1, 'ɛ'),
            % return the start node
            1;
        % call spawn
        {call, _, {atom, _, spawn}, [_, {atom, _, Name}, _]} ->
            VNew = common_fun:add_vertex(G),
            % todo: refactor
            SLabel = list_to_atom("spawn " ++ atom_to_list(Name)),
            digraph:add_edge(G, VLast, VNew, SLabel),
            VNew;
        % call of a generic function
        {call, _, {atom, _, Name}, _ArgList} ->
            % TODO capire bene cosa succede e perché funziona (caso più unico che raro)
            NewG = eval_func(Name),
            case NewG of
                no_graph ->
                    VLast;
                _ ->
                    merge_graph(G, NewG, VLast)
            end;
        % PMList = PatternMatchingList
        {'case', _, {var, _, Var}, PMList} ->
            parse_pm(PMList, G, VLast, FunName, atom_to_list(Var) ++ " match ");
        {'if', _, PMList} ->
            parse_pm(PMList, G, VLast, FunName, "if ");
        {'receive', _, PMList} ->
            parse_pm(PMList, G, VLast, FunName, "receive ");
        {op, _, '!', {_, _, VarOrAtomName}, {atom, _, DataSent}} ->
            VNew = common_fun:add_vertex(G),
            S = "send " ++ atom_to_list(DataSent) ++ " to " ++ atom_to_list(VarOrAtomName),
            digraph:add_edge(G, VLast, VNew, list_to_atom(S)),
            VNew;
        % se è un caso non coperto vado avanti
        _ ->
            VLast
    end.

merge_graph(G1, G2, VLast) ->
    VG2 = digraph:vertices(G2),
    % Add a number of vertex equal to VG2
    M = lists:foldl(
        fun(Item, M) ->
            maps:put(Item, common_fun:add_vertex(G1), M)
        end,
        maps:new(),
        VG2
    ),
    % link the first new state created to the main graph
    digraph:add_edge(G1, VLast, maps:get(1, M), 'ɛ'),
    % Add all the edges
    EG2 = digraph:edges(G2),
    lists:foreach(
        fun(Item) ->
            {Item, V1, V2, Label} = digraph:edge(G2, Item),
            digraph:add_edge(G1, maps:get(V1, M), maps:get(V2, M), Label)
        end,
        EG2
    ),
    % return last added vertex
    maps:get(lists:max(maps:keys(M)), M).

eval_func(FuncName) ->
    FunAst = common_fun:get_fun_ast_from_db(FuncName),
    case FunAst of
        no_ast_found ->
            no_graph;
        _ ->
            get_graph(FuncName, FunAst, false)
    end.

parse_pm(PMList, G, VLast, FunName, Label) ->
    List = explore_pm(PMList, G, VLast, FunName, Label),
    VR = common_fun:add_vertex(G),
    add_edges_recursive(G, List, VR, 'ɛ'),
    VR.

explore_pm(PMList, G, VLast, FunName, Base) ->
    lists:foldl(
        fun(CodeLine, AddedVertexList) ->
            case CodeLine of
                {clause, _, Vars, Guard, Content} ->
                    VNew = common_fun:add_vertex(G),
                    EdLabel = format_label_pm_edge(Vars, Guard, Base),
                    digraph:add_edge(G, VLast, VNew, list_to_atom(EdLabel)),
                    VRet = eval_pm_clause(Content, FunName, G, VNew),
                    AddedVertexList ++ [VRet];
                _ ->
                    AddedVertexList
            end
        end,
        [],
        PMList
    ).

format_label_pm_edge(VarList, GuardList, Label) when is_list(Label) ->
    VFun = fun(V, L) ->
        case V of
            {var, _, '_'} -> L ++ "_";
            {var, _, Var} -> L ++ atom_to_list(Var);
            {atom, _, Atom} -> L ++ atom_to_list(Atom);
            {nil, _} -> L ++ "null";
            _ -> L
        end
    end,
    GFun = fun(G, L) ->
        case G of
            {op, _, _, _} -> L ++ " (guards)";
            _ -> L
        end
    end,
    VarsString = lists:foldl(fun(V, Acc) -> VFun(V, Acc) end, Label, VarList),
    lists:foldl(fun(G, Acc) -> GFun(G, Acc) end, VarsString, GuardList).

add_edges_recursive(G, EdgeList, V, Label) ->
    [digraph:add_edge(G, Edge, V, Label) || Edge <- EdgeList, Edge =/= 1].

set_as_final(G, V) ->
    {_, LastLabel} = digraph:vertex(G, V),
    if
        (is_integer(LastLabel)) and (LastLabel =/= 1) ->
            FLabel = ?FINALSTATE ++ integer_to_list(LastLabel),
            digraph:add_vertex(G, V, FLabel);
        true ->
            ?UNDEFINED
    end.
