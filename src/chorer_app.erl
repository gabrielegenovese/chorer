%%%-------------------------------------------------------------------
%% @doc ChorEr public API
%% @end
%%%-------------------------------------------------------------------
-module(chorer_app).
-include("common.hrl").
-export([gen_chor/2]).

gen_chor(InputFile, OutputDir) ->
    io:format(
        "Input file: ~s~n"
        "Output directory: ~s~n",
        [InputFile, OutputDir]
    ),
    AST = get_ast(InputFile),
    generate_local_views(AST, OutputDir),
    generate_global_view(AST, OutputDir),
    AST.

%% internal functions

get_ast(File) ->
    {ok, AST} = epp_dodger:quick_parse_file(File),
    AST.

% Convert the graph to dot and save it into a file
save_to_file(S, Dir, File) ->
    Data = unicode:characters_to_binary(S),
    file:make_dir(Dir),
    FilePath = filename:join(Dir, format_name(File)),
    file:write_file(FilePath, Data).

explore_fun(L) when is_list(L) ->
    G = digraph:new(),
    V = digraph:add_vertex(G, 1, 1),
    explore_fun(L, G, V).
% return function
explore_fun([], G, VLast) ->
    {G, VLast};
% G = Graph, VLast = last vertex inserted
explore_fun([H | T], G, VLast) ->
    VNew = eval(H, G, VLast),
    explore_fun(T, G, VNew).

new_label(G) ->
    length(digraph:vertices(G)) + 1.

format_label_pm_edge([], [], Label) ->
    Label;
format_label_pm_edge([], [G1 | G], L) ->
    [H | _] = G1,
    case H of
        {op, _, _, _} ->
            NewL = L ++ " with guard";
        _ ->
            NewL = L
    end,
    format_label_pm_edge([], G, NewL);
format_label_pm_edge([V1 | V], [], L) ->
    case V1 of
        {var, _, '_'} ->
            NewL = L ++ "_";
        {var, _, Var} ->
            NewL = L ++ atom_to_list(Var);
        {atom, _, Atom} ->
            NewL = L ++ atom_to_list(Atom);
        _ ->
            NewL = L
    end,
    format_label_pm_edge([], V, NewL);
format_label_pm_edge(V, G, L) when is_list(L) ->
    NewL = format_label_pm_edge(V, [], L),
    format_label_pm_edge([], G, NewL).

explore_pm([], _, _, L) ->
    L;
explore_pm([H | T], G, VLast, L) ->
    case H of
        {clause, _, Vars, Guard, Content} ->
            VNew = add_vertex(G),
            EdLabel = format_label_pm_edge(Vars, Guard, "match: "),
            digraph:add_edge(G, VLast, VNew, list_to_atom(EdLabel)),
            {_, VRet} = explore_fun(Content, G, VNew),
            NewL = L ++ [VRet],
            explore_pm(T, G, VLast, NewL);
        _ ->
            explore_pm(T, G, VLast, L)
    end.

add_edges_recursive(_, [], _) ->
    ok;
add_edges_recursive(G, [H | T], V) ->
    digraph:add_edge(G, H, V),
    add_edges_recursive(G, T, V).

eval(L, G, VLast) ->
    case L of
        {clause, _, Vars, Guard, Content} ->
            VNew = add_vertex(G),
            EdLabel = format_label_pm_edge(Vars, Guard, "match: "),
            digraph:add_edge(G, VLast, VNew, list_to_atom(EdLabel)),
            {_, _VRet} = explore_fun(Content, G, VNew),
            VLast;
        {match, _, _RightContent, LeftContent} ->
            eval(LeftContent, G, VLast);
        {call, _, {atom, _, spawn}, _} ->
            VNew = add_vertex(G),
            digraph:add_edge(G, VLast, VNew, spawn),
            VNew;
        % PMList = PatternMatchingList
        {'receive', _, PMList} ->
            VNew = add_vertex(G),
            digraph:add_edge(G, VLast, VNew, 'receive'),
            List = explore_pm(PMList, G, VNew, []),
            VR = add_vertex(G),
            add_edges_recursive(G, List, VR),
            VR;
        % se è un caso non coperto vado avanti
        _ ->
            VLast
    end.

add_vertex(G) ->
    Label = new_label(G),
    digraph:add_vertex(G, Label, Label).

format_name(Name) ->
    atom_to_list(Name) ++ "_local_view.dot".

generate_local_views([], _) ->
    ok;
generate_local_views([H | T], OutputDir) ->
    eval_code_line(H, OutputDir),
    generate_local_views(T, OutputDir).

set_as_final(G, V) ->
    LastLabel = length(digraph:vertices(G)),
    FLabel = ?FINALSTATE ++ integer_to_list(LastLabel),
    digraph:add_vertex(G, V, FLabel).

eval_code_line(Line, Dir) ->
    case Line of
        {function, _, Name, Args, FunAst} ->
            {G, _VLast} = explore_fun(FunAst),
            % set_as_final(G, VLast),
            S = digraph_to_dot:convert(G, atom_to_list(Name), Args),
            save_to_file(S, Dir, Name);
        % estendere con attribute, serve?
        % necessario per linee non coperte
        _ ->
            nothing
    end.

generate_global_view(_AST, _OutputDir) ->
    % TODO
    ok.
