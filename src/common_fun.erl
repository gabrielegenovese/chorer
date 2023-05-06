-module(common_fun).
-export([save_to_file/4, add_vertex/1, new_label/1]).

% Convert the graph to dot and save it into a file
save_to_file(S, Dir, File, Type) ->
    Data = unicode:characters_to_binary(S),
    file:make_dir(Dir),
    case Type of
        local ->
            FilePath = filename:join(Dir, format_local_name(File));
        global ->
            FilePath = filename:join(Dir, format_global_name(File))
    end,
    file:write_file(FilePath, Data).

add_vertex(G) ->
    Label = common_fun:new_label(G),
    digraph:add_vertex(G, Label, Label).

new_label(G) ->
    length(digraph:vertices(G)) + 1.

format_local_name(Name) ->
    Name ++ "_local_view.dot".

format_global_name(Name) ->
    Name ++ "_global_view.dot".
