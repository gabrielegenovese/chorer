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
    fsa:syncronize(L).


%%% L'idea è di spawnare tot processi quanti grafi e fare gestire tutto a un server che in automatico
%%% mandi avanti i vari processi attraverso il message passing, in modo da simulare in tutto e per tutto
%%% l'esecuzione del programma (modo brigoso e difficile, se capisco come fare con il prodotto e la sincronizzazione
%%% è meglio)

%% IDEA SCARTATA
proc(G, VState) ->
    receive
        {Server, op} ->
            E = digraph:edges(G, VState),
            Server ! {self(), G, E},
            proc(G, VState);
        {Server, use, E} ->
            {E, VState, VNext, Label} = digraph:edge(G, E),
            Server ! {self(), G, Label},
            proc(G, VNext);
        {Server, get} ->
            Server ! {self(), G, VState},
            proc(G, VState)
    end.


