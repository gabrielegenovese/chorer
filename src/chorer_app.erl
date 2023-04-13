%%%-------------------------------------------------------------------
%% @doc ChorEr public API
%% @end
%%%-------------------------------------------------------------------

-module(chorer_app).

-behaviour(application).

-export([start/2, stop/1, gen_chor/2, get_ast/1]).

start(_StartType, _StartArgs) ->
    chorer_sup:start_link().

stop(_State) ->
    ok.

gen_chor(InputFile, OutputDir) ->
    io:format("Input file: ~s~nOutput directory ~s~n", [InputFile, OutputDir]),
    AST = get_ast(InputFile),
    generate_local_views(AST, OutputDir),
    generate_global_view(AST),
    AST.

%% internal functions

get_ast(File) ->
    {ok, AST} = epp_dodger:quick_parse_file(File),
    AST.

is_var(V) ->
    case V of
        {var, _, Name} -> Name;
        true -> false
    end.

is_atom_spaw(F) ->
    case F of
        {atom, _, spawn} -> true;
        _ -> false
    end.

is_spawn(V) ->
    case V of
        {call, N, {atom, _, spawn}, ArgList} ->
            io:fwrite("Spawn detected on line ~b~n", [N]),
            ArgList;
        true ->
            false
    end.

save_to_file(Graph, Dir, File) ->
    S = digraph_export:convert(Graph, dot),
    Data = unicode:characters_to_binary(S),
    file:make_dir(Dir),
    FilePath = filename:join(Dir, File),
    % io:fwrite("~s~n", [FilePath]),
    file:write_file(FilePath, Data).

explore_fun(L) when is_list(L) ->
    G = digraph:new(),
    V = digraph:add_vertex(G, 1, 1),
    explore_fun(L, G, V).
% return function
explore_fun([], G, _) ->
    G;
% G = Graph VE = vertex and edges
explore_fun([H | T], G, VLast) ->
    case H of
        {clause, _, _, _, Content} ->
            explore_fun(Content, G, VLast);
        {match, _, RightContent, LeftContent} ->
            % TODO: var può esserci o non esserci, gestire il caso
            _Var = is_var(RightContent),
            Args = is_spawn(LeftContent),
            if
                (Args =/= false) ->
                    S = length(digraph:vertices(G)) + 1,
                    VNew = digraph:add_vertex(G, S, S),
                    digraph:add_edge(G, VLast, VNew, spawn),
                    explore_fun(T, G, VNew);
                true ->
                    io:fwrite("Args false~n"),
                    explore_fun(T, G, VLast)
            end;
        {call, _, {atom, _, spawn}, _} ->
            S = length(digraph:vertices(G)) + 1,
            VNew = digraph:add_vertex(G, S, S),
            digraph:add_edge(G, VLast, VNew, spawn),
            explore_fun(T, G, VNew);
        % se è un caso non coperto vado avanti
        _ ->
            explore_fun(T, G, VLast)
    end.

format_name(Name) ->
    atom_to_list(Name) ++ ".dot".

generate_local_views([], _) ->
    ok;
generate_local_views([H | T], OutputDir) ->
    eval_code_line(H, OutputDir),
    generate_local_views(T, OutputDir).

eval_code_line(Line, Dir) ->
    case Line of
        {function, _, Name, _Args, FunAst} ->
            G = explore_fun(FunAst),
            save_to_file(G, Dir, format_name(Name));
        % estendere con gli altri costrutti?
        % necessario per linee non coperte
        _ ->
            nothing
    end.

generate_global_view(_AST) ->
    % TODO
    ok.
