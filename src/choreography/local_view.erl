-module(local_view).
-include("../common/common_data.hrl").
-export([generate/1]).

%% API
generate(OutputDir) ->
    CodeAst = common_fun:get_ast_from_db(),
    Fun = fun(CodeLine) -> eval_code_line(CodeLine, OutputDir) end,
    lists:foreach(Fun, CodeAst).

%% Internal Functions

eval_code_line(CodeLine, OutputDir) ->
    ActorList = common_fun:get_actors_from_db(),
    case CodeLine of
        {function, _, FunName, FunNArgs, FunAst} ->
            ?DBMANAGER ! {set_fun_ast, FunName, FunAst},
            IsActor = lists:member(FunName, ActorList),
            if
                IsActor ->
                    Gr = get_graph(FunName, FunAst),
                    fsa:minimize(Gr),
                    ?DBMANAGER ! {set_fun_graph, FunName, Gr},
                    SFunName = atom_to_list(FunName),
                    WriteData = digraph_to_dot:convert(Gr, SFunName, FunNArgs),
                    FileName = SFunName ++ integer_to_list(FunNArgs),
                    common_fun:save_to_file(WriteData, OutputDir, FileName, local),
                    #{FunName => Gr};
                true ->
                    none
            end;
        % necessario per linee non coperte
        _ ->
            none
    end.

get_graph(FunName, Code) when is_list(Code) ->
    Gr = digraph:new(),
    VStart = common_fun:add_vertex(Gr),
    lists:foreach(
        fun(Line) ->
            case Line of
                {clause, _, Vars, Guard, Content} ->
                    % TODO: cambiare l'ultimo argomento con un parametro da chiedere in input
                    VN = add_args_to_graph(Gr, Vars, Guard, VStart, false),
                    VFinal = eval_pm_function(Content, {FunName, Gr}, VN),
                    set_as_final(Gr, VFinal);
                Tmp ->
                    io:fwrite("No match in get_graph ~p~n", Tmp)
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

eval_pm_function(Code, Data, VStart) ->
    lists:foldl(
        fun(Line, VLast) ->
            eval_codeline(Line, Data, VLast)
        end,
        VStart,
        Code
    ).

eval_codeline(CodeLine, Data, VLast) ->
    {FunName, G} = Data,
    case CodeLine of
        %%% In a match code line (Var = ...), we just evaluate the left
        %%% content because it could be a receive, case, if, etc...
        %%% Future TODO: in the right content there could be a variable,
        %%% so we can save it and its content type (pid, atom, list, etc...) on the dbmanager
        {match, _, _RightContent, LeftContent} ->
            eval_codeline(LeftContent, Data, VLast);
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
            _VRet = eval_func(Name),
            Ret = merge_graph(G, Name, VLast),
            case Ret of
                no_graph_found ->
                    VLast;
                _ ->
                    Ret
            end;
        % PMList = PatternMatchingList
        {'case', _, {var, _, Var}, PMList} ->
            parse_pm(PMList, G, VLast, Data, atom_to_list(Var) ++ " match ");
        {'if', _, PMList} ->
            parse_pm(PMList, G, VLast, Data, "if ");
        {'receive', _, PMList} ->
            parse_pm(PMList, G, VLast, Data, "receive ");
        {op, _, '!', {_, _, VarOrAtomName}, {atom, _, DataSent}} ->
            VNew = common_fun:add_vertex(G),
            S = "send " ++ atom_to_list(DataSent) ++ " to " ++ atom_to_list(VarOrAtomName),
            digraph:add_edge(G, VLast, VNew, list_to_atom(S)),
            VNew;
        % se è un caso non coperto vado avanti
        _ ->
            VLast
    end.

merge_graph(G1, SecondFuncName, VLast) ->
    G2 = common_fun:get_fun_graph_from_db(SecondFuncName),
    case G2 of
        no_graph_found ->
            no_graph_found;
        _ ->
            VG2 = digraph:vertices(G2),
            EG2 = digraph:edges(G2),
            M = lists:foldl(
                fun(Item, M) ->
                    maps:put(Item, common_fun:add_vertex(G1), M)
                end,
                maps:new(),
                VG2
            ),
            digraph:add_edge(G1, VLast, maps:get(1, M), 'ɛ'),
            lists:foreach(
                fun(Item) ->
                    {Item, V1, V2, Label} = digraph:edge(G2, Item),
                    digraph:add_edge(G1, maps:get(V1, M), maps:get(V2, M), Label)
                end,
                EG2
            ),
            % return last added vertex
            maps:get(lists:max(maps:keys(M)), M)
    end.

eval_func(FuncName) ->
    G = digraph:new(),
    Ast = common_fun:get_ast_from_db(),
    VNew = lists:foldl(
        fun(Line, VLast) ->
            case Line of
                {function, _, Name, _, FunAst} ->
                    if
                        Name =:= FuncName -> get_fun_graph(FunAst, {Ast, Name, G});
                        true -> VLast
                    end;
                _ ->
                    VLast
            end
        end,
        1,
        Ast
    ),
    ?DBMANAGER ! {set_fun_graph, FuncName, G},
    VNew.

get_fun_graph(Code, Data) when is_list(Code) ->
    {_Ast, _Name, G} = Data,
    VStart = common_fun:add_vertex(G),
    lists:foldl(
        fun(Line, VLast) ->
            case Line of
                {clause, _, _, _, Content} ->
                    eval_pm_function(Content, Data, VLast);
                Tmp ->
                    io:fwrite("No match in get_graph ~p~n", Tmp),
                    VStart
            end
        end,
        VStart,
        Code
    ).

parse_pm(PMList, G, VLast, Data, Label) ->
    List = explore_pm(PMList, G, VLast, Data, Label),
    VR = common_fun:add_vertex(G),
    add_edges_recursive(G, List, VR, 'ɛ'),
    VR.

explore_pm(PMList, G, VLast, Data, Base) ->
    lists:foldl(
        fun(CodeLine, AddedVertexList) ->
            case CodeLine of
                {clause, _, Vars, Guard, Content} ->
                    VNew = common_fun:add_vertex(G),
                    EdLabel = format_label_pm_edge(Vars, Guard, Base),
                    digraph:add_edge(G, VLast, VNew, list_to_atom(EdLabel)),
                    VRet = eval_pm_function(Content, Data, VNew),
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
            none
    end.
