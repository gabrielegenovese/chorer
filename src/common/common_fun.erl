-module(common_fun).
-include("common_data.hrl").

%% API
-export([
    save_graph_to_file/4,
    add_vertex/1,
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

% Save some content into the specified directory with a formatted filename
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

%%%===================================================================
%%% Set of functions to interract with the dbmanager
%%%===================================================================

get_entrypoit_from_db() ->
    ?DBMANAGER ! {self(), get_entrypoint},
    recv().

get_ast_from_db() ->
    ?DBMANAGER ! {self(), get_ast},
    recv().

get_exported_fun_from_db() ->
    ?DBMANAGER ! {self(), get_exported_fun},
    recv().

get_actors_from_db() ->
    ?DBMANAGER ! {self(), get_actor_list},
    recv().

get_fun_ast_from_db(Key) ->
    ?DBMANAGER ! {self(), get_fun_ast, Key},
    recv().

get_fun_graph_from_db(Key) ->
    ?DBMANAGER ! {self(), get_fun_graph, Key},
    recv().

%%%===================================================================
%%% Internal Functions
%%%===================================================================

format_local_name(Name) ->
    Name ++ "_local_view.dot".

format_global_name(Name) ->
    Name ++ "_global_view.dot".

new_label(G) ->
    length(digraph:vertices(G)) + 1.

recv() ->
    receive
        {D} -> D
    end.