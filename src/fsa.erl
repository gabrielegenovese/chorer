-module(fsa).
-include("common_data.hrl").
-export([minimize/1]).

%%% API
minimize(G) ->
    % NFA to DFA
    remove_epsilon_moves(G),
    remove_unreachable(G),
    remove_nondistinguishable(G),
    done.

%%% internal functions

remove_epsilon_moves(G) ->
    E = digraph:edges(G),
    remove_epsilon_moves(G, E).

remove_epsilon_moves(_, []) ->
    done;
remove_epsilon_moves(G, [Edge | T]) ->
    EdgeInfo = digraph:edge(G, Edge),
    case EdgeInfo of
        {_, VToDel, VToLink, Label} ->
            case Label of
                'ɛ' ->
                    EL = digraph:edges(G, VToDel),
                    done = replace_epsilon_edges(G, EL, VToLink),
                    % this call will also delete all the edges linked to that vertex
                    digraph:del_vertex(G, VToDel),
                    % call remove_epsilon_moves with a new e dge list bacause, in replace_epsilon_edges, an edge is added and could be an epsilon transition
                    remove_epsilon_moves(G, digraph:edges(G));
                _ ->
                    remove_epsilon_moves(G, T)
            end;
        false ->
            digraph:del_edge(G, Edge),
            remove_epsilon_moves(G, T)
    end.

replace_epsilon_edges(_, [], _) ->
    done;
replace_epsilon_edges(G, [EToDel | T], VToLink) ->
    {_, V, _, Label} = digraph:edge(G, EToDel),
    digraph:add_edge(G, V, VToLink, Label),
    replace_epsilon_edges(G, T, VToLink).

remove_unreachable(G) ->
    VList = digraph:vertices(G),
    lists:foreach(
        fun(V) ->
            if
                % exclude start node
                V =/= 1 ->
                    N = length(digraph:in_neighbours(G, V)),
                    if
                        N =:= 0 -> digraph:del_vertex(G, V);
                        true -> nothing
                    end;
                true ->
                    nothing
            end
        end,
        VList
    ).

remove_nondistinguishable(_G) ->
    done.
