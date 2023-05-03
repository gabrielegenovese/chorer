-module(gen_global_view).
-include("common_data.hrl").
-export([generate/3]).

%% API
generate(List, OutputDir, EntryPoint) ->
    generate(List, List, OutputDir, EntryPoint).

%% Internal Functions

generate([], _, _, _) ->
    no_entry_point;
generate([{MainGraph, Name} | T], List, OutputDir, EntryPoint) ->
    if
        Name =:= EntryPoint ->
            generate_global(List, MainGraph, Name, OutputDir);
        true ->
            generate(T, List, OutputDir, EntryPoint)
    end.

generate_global(List, MainGraph, Name, Dir) ->
    Gr = get_graph(List, MainGraph),
    % done = minimize(Gr),
    Data = digraph_to_dot:convert(Gr),
    common_fun:save_to_file(Data, Dir, atom_to_list(Name), global),
    done.

get_graph(L, _G) ->
    fsa:product(L),
    fsa:presync(L),
    fsa:syncronize(L).