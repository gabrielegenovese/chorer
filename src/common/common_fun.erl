-module(common_fun).
-include("common_data.hrl").

%%% API
-export([
    save_graph_to_file/4,
    add_vertex/1,
    del_vertex/2,
    get_entrypoit_from_db/0,
    get_exported_fun_from_db/0,
    get_ast_from_db/0,
    get_actors_from_db/0,
    get_fun_ast_from_db/1,
    get_fun_graph_from_db/1
]).

%%%===================================================================
%%% API
%%%===================================================================

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
            GraphDotStr = digraph_to_dot:convert(Graph),
            FilePath = filename:join([Dir, format_global_name(FileName)])
    end,
    ToWriteData = unicode:characters_to_binary(GraphDotStr),
    file:make_dir(Dir),
    file:write_file(FilePath, ToWriteData).

-spec add_vertex(G) -> digraph:vertex() when
    G :: digraph:graph().
add_vertex(G) ->
    Label = new_label(G),
    digraph:add_vertex(G, Label, Label).

del_vertex(G, V) ->
    % TODO questa funzione cambia solo le label, l'ideale sarebbe cambiare anche il contenuto del vertice ma è difficile come funzione
    [digraph:add_vertex(G, Ver, Ver - 1) || Ver <- digraph:vertices(G), Ver > V],
    digraph:del_vertex(G, V).

%%%===================================================================
%%% Functions to interract with the dbmanager
%%%===================================================================

get_entrypoit_from_db() -> get_from_db({self(), get_entrypoint}).
get_ast_from_db() -> get_from_db({self(), get_ast}).
get_exported_fun_from_db() -> get_from_db({self(), get_exported_fun}).
get_actors_from_db() -> get_from_db({self(), get_actor_list}).
get_fun_ast_from_db(Key) -> get_from_db({self(), get_fun_ast, Key}).
get_fun_graph_from_db(Key) -> get_from_db({self(), get_fun_graph, Key}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

format_local_name(Name) ->
    Name ++ "_local_view.dot".

format_global_name(Name) ->
    Name ++ "_global_view.dot".

new_label(G) ->
    length(digraph:vertices(G)) + 1.

get_from_db(Data) ->
    ?DBMANAGER ! Data,
    recv().

recv() ->
    receive
        {D} -> D
    end.
