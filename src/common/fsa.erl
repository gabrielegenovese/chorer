-module(fsa).
-include("common_data.hrl").

%%% API
-export([minimize/1]).

%%%===================================================================
%%% API
%%%===================================================================

minimize(G) ->
    % convert a NFA to a DFA
    remove_epsilon_moves(G),
    % Minimization of the DFA
    remove_unreachable(G),
    remove_nondistinguishable(G).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

remove_epsilon_moves(G) ->
    EdgeList = digraph:edges(G),
    remove_epsilon_moves(G, EdgeList).

%%% TODO: capire correttezza algoritmo
%%%
%%% L'algoritmo potrebbe creare problemi con loop
%%% Attualmente crea problemi con le label

remove_epsilon_moves(_, []) ->
    done;
remove_epsilon_moves(G, [Edge | T]) ->
    EdgeInfo = digraph:edge(G, Edge),
    case EdgeInfo of
        {EpsEdgeToDel, VLeft, VRight, Label} ->
            case Label of
                'ɛ' ->
                    if
                        %%% If the Left Vertex is NOT the start node, then we can delete it
                        VLeft =/= 1 ->
                            %%% We want to delete the Left Vertex of the transaction
                            %%% But we also want to preserve all the other edges
                            EOut = digraph:out_edges(G, VLeft),
                            EIn = digraph:in_edges(G, VLeft),
                            %%% Any in edge of the Left Vertex is now an in edge of the Right Vertex
                            lists:foreach(
                                fun(EToAdd) ->
                                    {EToAdd, V, VLeft, ELabel} = digraph:edge(G, EToAdd),
                                    digraph:add_edge(G, V, VRight, ELabel)
                                end,
                                EIn
                            ),
                            %%% Same for the out edges
                            lists:foreach(
                                fun(EToAdd) ->
                                    if
                                        %%% Attention! We don't want to re-add the epsilon edge
                                        EToAdd =/= EpsEdgeToDel ->
                                            {EToAdd, VLeft, V, ELabel} = digraph:edge(G, EToAdd),
                                            digraph:add_edge(G, VRight, V, ELabel);
                                        true ->
                                            nothing
                                    end
                                end,
                                EOut
                            ),
                            %%% Finally, delete the vertex
                            digraph:del_vertex(G, VLeft);
                        %%% If the Left Vertex IS the start node, then we can't delete it
                        %%% We will delete the Right Vertex insted
                        VLeft =:= 1 ->
                            EOut = digraph:out_edges(G, VRight),
                            EIn = digraph:in_edges(G, VRight),
                            lists:foreach(
                                fun(EToAdd) ->
                                    {EToAdd, VRight, V, ELabel} = digraph:edge(G, EToAdd),
                                    digraph:add_edge(G, VLeft, V, ELabel)
                                end,
                                EOut
                            ),
                            lists:foreach(
                                fun(EToAdd) ->
                                    if
                                        EToAdd =/= EpsEdgeToDel ->
                                            {EToAdd, V, VRight, ELabel} = digraph:edge(G, EToAdd),
                                            digraph:add_edge(G, V, VLeft, ELabel);
                                        true ->
                                            nothing
                                    end
                                end,
                                EIn
                            ),
                            digraph:del_vertex(G, VRight)
                    end,
                    %%% We want to call remove_epsilon_moves with a new edge list because
                    %%% above we created and deleted many edges. So, we need a new edege list.
                    NewEdgeList = digraph:edges(G),
                    remove_epsilon_moves(G, NewEdgeList);
                _ ->
                    remove_epsilon_moves(G, T)
            end;
        false ->
            remove_epsilon_moves(G, T)
    end.

%%% if the number of incident edges on a vertex is 0, then delete it
remove_unreachable(G) ->
    VList = digraph:vertices(G),
    [
        digraph:del_vertex(G, V)
     || V <- VList,
        V =/= 1,
        digraph:in_degree(G, V) =:= 0
    ].

% Future TODO
remove_nondistinguishable(_G) ->
    ok.
