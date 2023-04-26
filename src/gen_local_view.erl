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
            Gr = get_graph(Name, FunAst),
            done = minimize(Gr),
            FunName = atom_to_list(Name),
            Data = digraph_to_dot:convert(Gr, FunName, Args),
            FileName = FunName ++ integer_to_list(Args),
            common_fun:save_to_file(Data, Dir, FileName, local);
        % necessario per linee non coperte
        _ ->
            unrecognized_line
    end.

% TODO fare meglio
minimize(G) ->
    Edges = digraph:edges(G),
    remove_epsilon_moves(G, Edges).

remove_epsilon_moves(_, []) ->
    done;
remove_epsilon_moves(G, [H | T]) ->
    E = digraph:edge(G, H),
    case E of
        {H, V1, V2, Label} ->
            if
                % do not delete start state
                V1 =/= 1 ->
                    case Label of
                        'ɛ' ->
                            EL = digraph:in_edges(G, V1),
                            done = replace_epsilon_edges(G, EL, V2);
                        _ ->
                            nothing
                    end;
                true ->
                    done
            end;
        false ->
            done
    end,
    remove_epsilon_moves(G, T).

replace_epsilon_edges(_, [], _) ->
    done;
replace_epsilon_edges(G, [H | T], V) ->
    {H, V1, V2, Label} = digraph:edge(G, H),
    digraph:add_edge(G, V1, V, Label),
    true = digraph:del_edge(G, H),
    digraph:del_vertex(G, V2),
    replace_epsilon_edges(G, T, V).

get_graph(FunName, Code) when is_list(Code) ->
    Gr = digraph:new(),
    % Data = #node_data{is_start = true, current_operation = init, label = 1},
    VStart = digraph:add_vertex(Gr, 1, 1),
    get_graph(Code, Gr, FunName, VStart).

get_graph([], G, _, _) ->
    G;
% G = Graph, VLast = last vertex inserted
get_graph([H | T], G, FunName, VStart) ->
    case H of
        {clause, _, Vars, Guard, Content} ->
            VN = add_vertex(G),
            EdLabel = format_label_pm_edge(Vars, Guard, "match "),
            digraph:add_edge(G, list_to_atom(EdLabel), VStart, VN, 'ɛ'),
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
            parse_pm(PMList, G, VLast, FunName, atom_to_list(Var) ++ " case ");
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
