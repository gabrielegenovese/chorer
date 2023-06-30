-module(common_fun).
-include("common_data.hrl").

%%% API
-export([
    first/1,
    pick_random/1,
    save_graph_to_file/4,
    add_vertex/1,
    del_vertex/2
]).

%%%===================================================================
%%% API
%%%===================================================================

%%% Pick a random element from a list
pick_random(X) -> lists:nth(rand:uniform(length(X)), X).

%%% Pick first element from a list
first([]) -> [];
first([H | _]) -> H.

%%% Save some content into the specified directory with a formatted filename
-spec save_graph_to_file(Graph, Dir, FileName, Type) -> ok when
    Graph :: digraph:graph(),
    Dir :: string(),
    FileName :: string(),
    Type :: atom().
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

%%% Add a vertex to a FSA
-spec add_vertex(G) -> digraph:vertex() when G :: digraph:graph().
add_vertex(G) ->
    Label = new_label(G),
    digraph:add_vertex(G, Label, Label).

%%% Delete a vertex to a FSA
del_vertex(G, V) ->
    % TODO questa funzione cambia solo le label, l'ideale sarebbe cambiare anche il contenuto del vertice ma è difficile da fare
    [digraph:add_vertex(G, Ver, Ver - 1) || Ver <- digraph:vertices(G), Ver > V],
    digraph:del_vertex(G, V).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec format_local_name(Name) -> string() when Name :: string().
format_local_name(Name) -> Name ++ "_local_view.dot".

-spec format_global_name(Name) -> string() when Name :: string().
format_global_name(Name) -> Name ++ "_global_view.dot".

%%% Get a new label for a given graph
new_label(Graph) -> length(digraph:vertices(Graph)) + 1.
