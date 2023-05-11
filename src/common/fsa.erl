-module(fsa).
-include("common_data.hrl").

%%% API
-export([minimize/1]).

%%%===================================================================
%%% API
%%%===================================================================

minimize(G) ->
    % NFA to DFA
    remove_epsilon_moves(G),
    % Minimization
    remove_unreachable(G),
    remove_nondistinguishable(G).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

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
                    if
                        VToDel =:= 1 ->
                            EL = digraph:out_edges(G, VToLink),
                            lists:foreach(
                                fun(EToDel) ->
                                    {_, _, V, ELabel} = digraph:edge(G, EToDel),
                                    digraph:add_edge(G, VToDel, V, ELabel)
                                end,
                                EL
                            ),
                            digraph:del_vertex(G, VToLink),
                            remove_epsilon_moves(G, digraph:edges(G));
                        true ->
                            EL = digraph:in_edges(G, VToDel),
                            % for each edge in the list create a copy of it but with different vertex
                            lists:foreach(
                                fun(EToDel) ->
                                    {_, V, _, ELabel} = digraph:edge(G, EToDel),
                                    digraph:add_edge(G, V, VToLink, ELabel)
                                end,
                                EL
                            ),
                            % this call will delete also all the edges attached to the vertex

                            digraph:del_vertex(G, VToDel),
                            % recursive call with new edge list bacause edges could be added and there could be an epsilon transition
                            remove_epsilon_moves(G, digraph:edges(G))
                    end;
                _ ->
                    remove_epsilon_moves(G, T)
            end;
        false ->
            % digraph:del_edge(G, Edge),
            remove_epsilon_moves(G, T)
    end.

%%% if the number of edges incident on that vertex is 0, then delete the vertex
remove_unreachable(G) ->
    VList = digraph:vertices(G),
    [
        digraph:del_vertex(G, V)
     || V <- VList,
        V =/= 1,
        digraph:in_degree(G, V) =:= 0
    ].

remove_nondistinguishable(_G) ->
    ok.
