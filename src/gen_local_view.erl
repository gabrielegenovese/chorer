-module(gen_local_view).
-include("common_data.hrl").
-export([generate/3]).

%% API
generate(L, OutputDir, EntryPoint) ->
    ActorList = gen_actor_list(L, EntryPoint),
    generate(L, L, OutputDir, ActorList, maps:new()).

%% Internal Functions

generate([], _, _, _, L) ->
    L;
generate([H | T], Ast, OutputDir, ActorList, Map) ->
    GM = eval_code_line(H, {Ast, OutputDir, ActorList}),
    generate(T, Ast, OutputDir, ActorList, maps:merge(Map, GM)).

gen_actor_list(L, EP) ->
    gen_actor_list(L, EP, [EP]).

gen_actor_list([], _, L) ->
    L;
gen_actor_list([H | T], EP, L) ->
    NewL =
        case H of
            {function, _, Name, _, FunAst} ->
                if
                    Name =:= EP ->
                        L ++ find_actor(FunAst);
                    true ->
                        L
                end;
            _ ->
                L
        end,
    gen_actor_list(T, EP, NewL).

find_actor(L) ->
    find_actor(L, []).

find_actor([], L) ->
    L;
find_actor([H | T], L) ->
    NewL =
        case H of
            {clause, _, _, _, Content} ->
                find_actor(Content, L);
            {match, _, _, LeftContent} ->
                find_actor([LeftContent], L);
            {call, _, {atom, _, spawn}, Args} ->
                {atom, _, Name} = lists:nth(2, Args),
                L ++ [Name];
            % PMList = PatternMatchingList
            {'case', _, _, PMList} ->
                find_actor(PMList, L);
            {'if', _, PMList} ->
                find_actor(PMList, L);
            {'receive', _, PMList} ->
                find_actor(PMList, L);
            _ ->
                L
        end,
    find_actor(T, NewL).

eval_code_line(Line, Data) ->
    {Ast, Dir, ActorList} = Data,
    case Line of
        {function, _, Name, Args, FunAst} ->
            IsActor = lists:member(Name, ActorList),
            if
                IsActor ->
                    Gr = get_graph(FunAst, {Ast, Name}),
                    done = fsa:minimize(Gr),
                    FunName = atom_to_list(Name),
                    WData = digraph_to_dot:convert(Gr, FunName, Args),
                    FileName = FunName ++ integer_to_list(Args),
                    common_fun:save_to_file(WData, Dir, FileName, local),
                    #{Name => Gr};
                true ->
                    #{}
            end;
        % necessario per linee non coperte
        _ ->
            #{}
    end.

get_graph(Code, Data) when is_list(Code) ->
    Gr = digraph:new(),
    VStart = common_fun:add_vertex(Gr),
    get_graph(Code, Gr, Data, VStart).

get_graph([], G, _, _) ->
    G;
get_graph([H | T], G, Data, VStart) ->
    case H of
        {clause, _, Vars, Guard, Content} ->
            VN = common_fun:add_vertex(G),
            EdLabel = format_label_pm_edge(Vars, Guard, "arg "),
            digraph:add_edge(G, VStart, VN, list_to_atom(EdLabel)),
            VFinal = eval_pm_function(Content, G, Data, VN),
            set_as_final(G, VFinal);
        Tmp ->
            io:fwrite("No match in get_graph ~p~n", Tmp)
    end,
    get_graph(T, G, Data, VStart).

get_fun_graph(Code, D) when is_list(Code) ->
    {Ast, Name, G} = D,
    VStart = common_fun:add_vertex(G),
    get_fun_graph(Code, G, {Ast, Name}, VStart).

get_fun_graph([], _, _, V) ->
    V;
get_fun_graph([H | T], G, Data, VStart) ->
    VNew =
        case H of
            {clause, _, _, _, Content} ->
                eval_pm_function(Content, G, Data, VStart);
            Tmp ->
                io:fwrite("No match in get_graph ~p~n", Tmp),
                VStart
        end,
    get_fun_graph(T, G, Data, VNew).

eval_pm_function([], _, _, VLast) ->
    VLast;
eval_pm_function([H | T], G, Data, VLast) ->
    VNew = eval(H, G, Data, VLast),
    eval_pm_function(T, G, Data, VNew).

eval(L, G, Data, VLast) ->
    {Ast, FunName} = Data,
    case L of
        {match, _, _RightContent, LeftContent} ->
            VNew = eval(LeftContent, G, Data, VLast),
            VNew;
        % recursion
        {call, _, {atom, _, FunName}, _} ->
            digraph:add_edge(G, VLast, 1, 'ɛ'),
            % return to start
            1;
        % call spawn
        {call, _, {atom, _, spawn}, [_, {atom, _, Name}, _]} ->
            VNew = common_fun:add_vertex(G),
            % todo: refactor
            digraph:add_edge(G, VLast, VNew, list_to_atom("spawn " ++ atom_to_list(Name))),
            VNew;
        % call a genericfunction
        {call, _, {atom, _, Name}, _ArgList} ->
            % todo capire perché funziona (caso più unico che raro)
            {NewG, _VNew} = get_func_graph(Ast, Name),
            merge_graph(G, NewG, VLast);
        % PMList = PatternMatchingList
        {'case', _, {var, _, Var}, PMList} ->
            parse_pm(PMList, G, VLast, Data, atom_to_list(Var) ++ " match ");
        {'if', _, PMList} ->
            parse_pm(PMList, G, VLast, Data, "if ");
        {'receive', _, PMList} ->
            parse_pm(PMList, G, VLast, Data, "receive ");
        % send of atom supported, send of variabile ignored
        % TODO: implementare anche la send di variabili e tuple
        {op, _, '!', {var, _, Var}, {atom, _, DataSent}} ->
            VNew = common_fun:add_vertex(G),
            S = "send " ++ atom_to_list(DataSent) ++ " to " ++ atom_to_list(Var),
            digraph:add_edge(G, VLast, VNew, list_to_atom(S)),
            VNew;
        % se è un caso non coperto vado avanti
        _ ->
            VLast
    end.

for_each_merge_graph(L, G) ->
    for_each_merge_graph(L, G, maps:new()).
for_each_merge_graph([], _, M) ->
    M;
for_each_merge_graph([H | T], G, M) ->
    NewM = maps:put(H, common_fun:add_vertex(G), M),
    for_each_merge_graph(T, G, NewM).

merge_graph(G1, G2, VLast) ->
    VG2 = digraph:vertices(G2),
    EG2 = digraph:edges(G2),
    M = for_each_merge_graph(VG2, G1),
    digraph:add_edge(G1, VLast, maps:get(1, M), 'ɛ'),
    lists:foreach(
        fun(Item) ->
            {Item, V1, V2, Label} = digraph:edge(G2, Item),
            digraph:add_edge(G1, maps:get(V1, M), maps:get(V2, M), Label)
        end,
        EG2
    ),
    % return last added vertex
    maps:get(lists:max(maps:keys(M)), M).

get_func_graph(Ast, Fun) ->
    G = digraph:new(),
    get_func_graph(Ast, Ast, Fun, G, 1).

get_func_graph([], _, _, G, V) ->
    {G, V};
get_func_graph([H | T], Ast, Fun, G, VLast) ->
    VNew =
        case H of
            {function, _, Name, _, FunAst} ->
                if
                    Name =:= Fun ->
                        get_fun_graph(FunAst, {Ast, Name, G});
                    true ->
                        VLast
                end;
            % necessario per linee non coperte
            _ ->
                VLast
        end,
    get_func_graph(T, Ast, Fun, G, VNew).

parse_pm(PMList, G, VLast, Data, Label) ->
    List = explore_pm(PMList, G, VLast, [], Data, Label),
    VR = common_fun:add_vertex(G),
    add_edges_recursive(G, List, VR, 'ɛ'),
    VR.

explore_pm([], _, _, L, _, _) ->
    L;
explore_pm([H | T], G, VLast, L, Data, Base) ->
    NewL =
        case H of
            {clause, _, Vars, Guard, Content} ->
                VNew = common_fun:add_vertex(G),
                EdLabel = format_label_pm_edge(Vars, Guard, Base),
                digraph:add_edge(G, VLast, VNew, list_to_atom(EdLabel)),
                VRet = eval_pm_function(Content, G, Data, VNew),
                NL = L ++ [VRet],
                NL;
            _ ->
                L
        end,
    explore_pm(T, G, VLast, NewL, Data, Base).

format_label_pm_edge([], [], Label) ->
    Label;
format_label_pm_edge([], [G1 | G], L) ->
    [H | _] = G1,
    NewL =
        case H of
            {op, _, _, _} -> L ++ " (guards)";
            _ -> L
        end,
    format_label_pm_edge([], G, NewL);
format_label_pm_edge([V1 | V], [], L) ->
    NewL =
        case V1 of
            {var, _, '_'} -> L ++ "_";
            {var, _, Var} -> L ++ atom_to_list(Var);
            {atom, _, Atom} -> L ++ atom_to_list(Atom);
            {nil, _} -> L ++ "null";
            _ -> L
        end,
    format_label_pm_edge(V, [], NewL);
format_label_pm_edge(V, G, L) when is_list(L) ->
    NewL = format_label_pm_edge(V, [], L),
    format_label_pm_edge([], G, NewL).

add_edges_recursive(_, [], _, _) ->
    ok;
add_edges_recursive(G, [H | T], V, Label) ->
    if
        % if H is not the start vertex
        H =/= 1 -> digraph:add_edge(G, H, V, Label);
        true -> nothing
    end,
    add_edges_recursive(G, T, V, Label).

set_as_final(G, V) ->
    {_, LastLabel} = digraph:vertex(G, V),
    if
        (is_integer(LastLabel)) and (LastLabel =/= 1) ->
            FLabel = ?FINALSTATE ++ integer_to_list(LastLabel),
            digraph:add_vertex(G, V, FLabel);
        true ->
            nothing
    end.
