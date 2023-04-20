-module(gen_local_view).
-include("common_data.hrl").
-export([generate/2]).

%% API

generate([], _) ->
    ok;
generate([H | T], OutputDir) ->
    eval_code_line(H, OutputDir),
    generate(T, OutputDir).

%% Internal Functions

eval_code_line(Line, Dir) ->
    case Line of
        {function, _, Name, Args, FunAst} ->
            {G, _} = explore_fun(Name, FunAst),
            FunName = atom_to_list(Name),
            S = digraph_to_dot:convert(G, FunName, Args),
            FileName = FunName ++ integer_to_list(Args),
            common_fun:save_to_file(S, Dir, FileName, local);
        % necessario per linee non coperte
        _ ->
            unrecognized_line
    end.

explore_fun(FunName, L) when is_list(L) ->
    G = digraph:new(),
    V = digraph:add_vertex(G, 1, 1),
    explore_fun(FunName, L, G, V).
% return function
explore_fun(_, [], G, VLast) ->
    {G, VLast};
% G = Graph, VLast = last vertex inserted
explore_fun(FunName, [H | T], G, VLast) ->
    VNew = eval(FunName, H, G, VLast),
    explore_fun(FunName, T, G, VNew).

explore_pm([], _, _, L, _) ->
    L;
explore_pm([H | T], G, VLast, L, FunName) ->
    case H of
        {clause, _, Vars, Guard, Content} ->
            VNew = add_vertex(G),
            EdLabel = format_label_pm_edge(Vars, Guard, "match: "),
            digraph:add_edge(G, VLast, VNew, list_to_atom(EdLabel)),
            {_, VRet} = explore_fun(FunName, Content, G, VNew),
            NewL = L ++ [VRet],
            explore_pm(T, G, VLast, NewL, FunName);
        _ ->
            explore_pm(T, G, VLast, L, FunName)
    end.

eval(FunName, L, G, VLast) ->
    case L of
        {clause, _, Vars, Guard, Content} ->
            VNew = add_vertex(G),
            EdLabel = format_label_pm_edge(Vars, Guard, "match: "),
            digraph:add_edge(G, VLast, VNew, list_to_atom(EdLabel)),
            {_, VFinal} = explore_fun(FunName, Content, G, VNew),
            set_as_final(G, VFinal),
            VLast;
        {match, _, _RightContent, LeftContent} ->
            VNew = eval(FunName, LeftContent, G, VLast),
            VNew;
        {call, _, {atom, _, FunName}, _} ->
            digraph:add_edge(G, VLast, 1, loop),
            1;
        {call, _, {atom, _, spawn}, _} ->
            VNew = add_vertex(G),
            digraph:add_edge(G, VLast, VNew, spawn),
            VNew;
        % PMList = PatternMatchingList
        {'receive', _, PMList} ->
            VNew = add_vertex(G),
            digraph:add_edge(G, VLast, VNew, 'receive'),
            List = explore_pm(PMList, G, VNew, [], FunName),
            VR = add_vertex(G),
            add_edges_recursive(G, List, VR),
            VR;
        {op, _, '!', {var, _, Var}, _DataSent} ->
            VNew = add_vertex(G),
            S = "send to " ++ atom_to_list(Var),
            digraph:add_edge(G, VLast, VNew, list_to_atom(S)),
            VNew;
        % se è un caso non coperto vado avanti
        _ ->
            VLast
    end.

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
    format_label_pm_edge([], V, NewL);
format_label_pm_edge(V, G, L) when is_list(L) ->
    NewL = format_label_pm_edge(V, [], L),
    format_label_pm_edge([], G, NewL).

add_edges_recursive(_, [], _) ->
    ok;
add_edges_recursive(G, [H | T], V) ->
    if
        % if H is not the start vertex
        H =/= 1 ->
            digraph:add_edge(G, H, V);
        true ->
            nothing
    end,
    add_edges_recursive(G, T, V).

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
