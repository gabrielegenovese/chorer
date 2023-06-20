-module(common_fun).
-include("common_data.hrl").

%%% API
-export([
    first/1,
    pick_random/1,
    save_graph_to_file/4,
    add_vertex/1,
    del_vertex/2,
    get_entrypoit_from_db/0,
    get_exported_fun_from_db/0,
    get_ast_from_db/0,
    get_actors_from_db/0,
    get_fun_ast_from_db/1,
    get_fun_graph_from_db/1,
    add_reg_entry_from_db/1,
    get_reg_entry_from_db/0
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
            GraphDotStr = digraph_to_dot:convert(Graph),
            FilePath = filename:join([Dir, format_global_name(FileName)])
    end,
    ToWriteData = unicode:characters_to_binary(GraphDotStr),
    file:make_dir(Dir),
    file:write_file(FilePath, ToWriteData).

%%% Add a vertex to a FSA
-spec add_vertex(G) -> digraph:vertex() when
    G :: digraph:graph().
add_vertex(G) ->
    Label = new_label(G),
    digraph:add_vertex(G, Label, Label).

%%% Delete a vertex to a FSA
del_vertex(G, V) ->
    % TODO questa funzione cambia solo le label, l'ideale sarebbe cambiare anche il contenuto del vertice ma è difficile da fare
    [digraph:add_vertex(G, Ver, Ver - 1) || Ver <- digraph:vertices(G), Ver > V],
    digraph:del_vertex(G, V).

%%%===================================================================
%%% API to interract with the dbmanager
%%%===================================================================

get_entrypoit_from_db() -> get_from_db({self(), get_entrypoint}).
get_ast_from_db() -> get_from_db({self(), get_ast}).
get_exported_fun_from_db() -> get_from_db({self(), get_exported_fun}).
get_actors_from_db() -> get_from_db({self(), get_actor_list}).
get_fun_ast_from_db(Key) -> get_from_db({self(), get_fun_ast, Key}).
get_fun_graph_from_db(Key) -> get_from_db({self(), get_fun_graph, Key}).
add_reg_entry_from_db(L) -> get_from_db({self(), add_to_register_list, L}).
get_reg_entry_from_db() -> get_from_db({self(), get_register_list}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec format_local_name(Name) -> string() when Name :: string().
format_local_name(Name) -> Name ++ "_local_view.dot".

-spec format_global_name(Name) -> string() when Name :: string().
format_global_name(Name) -> Name ++ "_global_view.dot".

%%% Get a new label for a given graph
new_label(Graph) -> length(digraph:vertices(Graph)) + 1.

%%% Sent and receive data from the db menager
get_from_db(Data) ->
    ?DBMANAGER ! Data,
    recv().

%%% Receive data from the db menager
recv() ->
    receive
        {D} -> D
    end.
