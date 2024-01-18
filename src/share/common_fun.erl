-module(common_fun).
-include("common_data.hrl").

%%% API
-export([
    first/1,
    pick_random/1,
    save_graph_to_file/4,
    add_vertex/1,
    del_vertex/2,
    is_erlvar/1,
    is_uppercase/1,
    is_lowercase/1,
    get_fun_ast/1,
    get_localview/1,
    atol/1,
    ltoa/1
]).

%%%===================================================================
%%% API
%%%===================================================================

%%% Pick a random element from a list
pick_random(X) -> lists:nth(rand:uniform(length(X)), X).

%%% Pick first element from a list
first([]) -> [];
first([H | _]) -> H.

save_graph_to_file(Graph, Dir, FileName, Type) ->
    case Type of
        local ->
            GraphDotStr = digraph_to_dot:convert(Graph, FileName),
            FilePath = filename:join([Dir, format_local_name(FileName)]);
        global ->
            GraphDotStr = digraph_to_dot:convert(Graph, "global"),
            FilePath = filename:join([Dir, format_global_name(FileName)])
    end,
    ToWriteData = unicode:characters_to_binary(GraphDotStr),
    file:make_dir(Dir),
    file:write_file(FilePath, ToWriteData).

-spec add_vertex(G) -> digraph:vertex() when G :: digraph:graph().
add_vertex(G) ->
    Label = new_label(G),
    digraph:add_vertex(G, Label, Label).

del_vertex(G, V) ->
    % TODO questa funzione cambia solo le label, l'ideale sarebbe cambiare anche il contenuto del vertice ma è difficile da fare
    [digraph:add_vertex(G, Ver, Ver - 1) || Ver <- digraph:vertices(G), Ver > V],
    digraph:del_vertex(G, V).

%%% Return true if first letter's atom is uppercase (it's a variable in erlang), otherwise false.
is_erlvar(Name) ->
    SName = atol(Name),
    [FirstChar | _] = SName,
    is_uppercase([FirstChar]).

%%% If the input character is uppercase return true, otherwise false.
is_uppercase(Char) when
    (is_list(Char)) and (length(Char) =:= 1)
->
    (Char >= "A") and (Char =< "Z").

%%% If the input character is lowercase return true, otherwise false.
is_lowercase(Char) when
    (is_list(Char)) and (length(Char) =:= 1)
->
    (Char >= "a") and (Char =< "z").

get_fun_ast(FunName) ->
    Ast = ets:lookup(?FUNAST, FunName),
    case Ast of
        [] -> not_found;
        [{_, A}] -> A
    end.

get_localview(FunName) ->
    Ast = ets:lookup(?LOCALVIEW, FunName),
    case Ast of
        [] -> not_found;
        [{_, A}] -> A
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec format_local_name(Name) -> string() when Name :: string().
format_local_name(Name) -> Name ++ "_local_view.dot".

-spec format_global_name(Name) -> string() when Name :: string().
format_global_name(Name) -> Name ++ "_global_view.dot".

%%% Get a new label for a given graph
new_label(Graph) -> length(digraph:vertices(Graph)) + 1.

ltoa(L) when is_list(L) -> list_to_atom(L);
ltoa(L) when is_atom(L) -> L.
atol(A) when is_atom(A) -> atom_to_list(A);
atol(A) when is_list(A) -> A.
