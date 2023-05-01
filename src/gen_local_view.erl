-module(gen_local_view).
-include("common_data.hrl").
-export([generate/2]).

%% API
generate([H | T], OutputDir) ->
    generate([H | T], OutputDir, []).

%% Internal Functions

generate([], _, L) ->
    L;
generate([H | T], OutputDir, L) ->
    G = eval_code_line(H, OutputDir),
    NewL =
        if
            G =:= [] -> L;
            true -> L ++ G
        end,
    generate(T, OutputDir, NewL).

eval_code_line(Line, Dir) ->
    case Line of
        {function, _, Name, Args, FunAst} ->
            Gr = get_graph(Name, FunAst),
            io:fwrite("Name: ~p~n", [Name]),
            done = fsa:minimize(Gr),
            FunName = atom_to_list(Name),
            Data = digraph_to_dot:convert(Gr, FunName, Args),
            FileName = FunName ++ integer_to_list(Args),
            common_fun:save_to_file(Data, Dir, FileName, local),
            [Gr];
        % necessario per linee non coperte
        _ ->
            []
    end.

get_graph(FunName, Code) when is_list(Code) ->
    Gr = digraph:new(),
    % Data = #node_data{is_start = true, current_operation = init, label = 1},
    VStart = digraph:add_vertex(Gr, 1, 1),
    get_graph(Code, Gr, FunName, VStart).

get_graph([], G, _, _) ->
    G;
get_graph([H | T], G, FunName, VStart) ->
    case H of
        {clause, _, Vars, Guard, Content} ->
            VN = add_vertex(G),
            EdLabel = format_label_pm_edge(Vars, Guard, "arg "),
            digraph:add_edge(G, VStart, VN, list_to_atom(EdLabel)),
            VFinal = eval_pm_function(Content, G, FunName, VN),
            set_as_final(G, VFinal);
        Tmp ->
            io:fwrite("No match in get_graph ~p~n", Tmp)
    end,
    get_graph(T, G, FunName, VStart).

eval_pm_function([], _, _, VLast) ->
    VLast;
eval_pm_function([H | T], G, FunName, VLast) ->
    VNew = eval(H, G, FunName, VLast),
    eval_pm_function(T, G, FunName, VNew).

eval(L, G, FunName, VLast) ->
    case L of
        {match, _, _RightContent, LeftContent} ->
            VNew = eval(LeftContent, G, FunName, VLast),
            VNew;
        % recursion
        {call, _, {atom, _, FunName}, _} ->
            digraph:add_edge(G, VLast, 1, 'ɛ'),
            % return to start
            1;
        {call, _, {atom, _, spawn}, _} ->
            VNew = add_vertex(G),
            digraph:add_edge(G, VLast, VNew, spawn),
            VNew;
        % PMList = PatternMatchingList
        {'case', _, {var, _, Var}, PMList} ->
            parse_pm(PMList, G, VLast, FunName, atom_to_list(Var) ++ " match ");
        {'if', _, PMList} ->
            parse_pm(PMList, G, VLast, FunName, "if ");
        {'receive', _, PMList} ->
            parse_pm(PMList, G, VLast, FunName, "receive ");
        {op, _, '!', {var, _, Var}, _DataSent} ->
            VNew = add_vertex(G),
            S = "send to " ++ atom_to_list(Var),
            digraph:add_edge(G, VLast, VNew, list_to_atom(S)),
            VNew;
        % se è un caso non coperto vado avanti
        _ ->
            VLast
    end.

parse_pm(PMList, G, VLast, FunName, Label) ->
    List = explore_pm(PMList, G, VLast, [], FunName, Label),
    VR = add_vertex(G),
    add_edges_recursive(G, List, VR, 'ɛ'),
    VR.

explore_pm([], _, _, L, _, _) ->
    L;
explore_pm([H | T], G, VLast, L, FunName, Base) ->
    NewL =
        case H of
            {clause, _, Vars, Guard, Content} ->
                VNew = add_vertex(G),
                EdLabel = format_label_pm_edge(Vars, Guard, Base),
                digraph:add_edge(G, VLast, VNew, list_to_atom(EdLabel)),
                VRet = eval_pm_function(Content, G, FunName, VNew),
                NL = L ++ [VRet],
                NL;
            _ ->
                L
        end,
    explore_pm(T, G, VLast, NewL, FunName, Base).

format_label_pm_edge([], [], Label) ->
    Label;
format_label_pm_edge([], [G1 | G], L) ->
    [H | _] = G1,
    NewL =
        case H of
            {op, _, _, _} ->
                L ++ " (guards)";
            _ ->
                L
        end,
    format_label_pm_edge([], G, NewL);
format_label_pm_edge([V1 | V], [], L) ->
    NewL =
        case V1 of
            {var, _, '_'} ->
                L ++ "_";
            {var, _, Var} ->
                L ++ atom_to_list(Var);
            {atom, _, Atom} ->
                L ++ atom_to_list(Atom);
            {nil, _} ->
                L ++ "null";
            _ ->
                L
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
        H =/= 1 ->
            digraph:add_edge(G, H, V, Label);
        true ->
            nothing
    end,
    add_edges_recursive(G, T, V, Label).

add_vertex(G) ->
    Label = common_fun:new_label(G),
    digraph:add_vertex(G, Label, Label).

set_as_final(G, V) ->
    {_, LastLabel} = digraph:vertex(G, V),
    if
        (is_integer(LastLabel)) and (LastLabel =/= 1) ->
            FLabel = ?FINALSTATE ++ integer_to_list(LastLabel),
            digraph:add_vertex(G, V, FLabel);
        true ->
            nothing
    end.
