%%%===================================================================
%%% @doc
%%% This module manage an FSA.
%%% A choreography is basically an FSA (finite state automaton). This module
%%% implements the basics automata's operations needed for the project.
%%% @end
%%%===================================================================
-module(fsa).
-include("common_data.hrl").

%%% API
-export([minimize/1, rename_states/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Minimize an FSA, using algorithms from Simone Martini and Maurizio Gabbrielli's
%%% book: Programming Languages, Second edition, McGraw-Hill, 2011
-spec minimize(NFA) -> digraph:graph() when
    NFA :: digraph:graph().
minimize(NFA) ->
    % Convert a NFA to a DFA
    remove_unreachable(NFA),
    DFA = subset_construction(NFA),
    % Minimization of the DFA
    remove_unreachable(DFA),
    remove_nondistinguishable(DFA),
    rename_states(DFA),
    DFA.

%%%===================================================================
%%% Internal Functions
%%%
%%% NOTE: every "set of states" is an ordset (sorted list without
%%% duplicates). Unlike sets:set(), an ordset has a canonical
%%% representation, so two equal ordsets are always =:= and can be safely
%%% used as map keys or compared/looked up.
%%%===================================================================

%%% Epsilon closure of a list of states: all the states reachable from them
%%% using only ɛ transitions (the states themselves are included).
%%% Returns an ordset.
eps_clos(Graph, VertexL) when is_list(VertexL) ->
    eps_clos(Graph, VertexL, ordsets:new()).

eps_clos(_Graph, [], Seen) ->
    Seen;
eps_clos(Graph, [V | Rest], Seen) ->
    case ordsets:is_element(V, Seen) of
        true ->
            eps_clos(Graph, Rest, Seen);
        false ->
            Next = [
                V2
             || E <- digraph:out_edges(Graph, V),
                {_, _, V2, 'ɛ'} <- [digraph:edge(Graph, E)]
            ],
            eps_clos(Graph, Next ++ Rest, ordsets:add_element(V, Seen))
    end.

%%% Given a DFA state (an ordset of NFA states) returns, for every label
%%% different from ɛ, the DFA state reached with that label:
%%% [{Label, TargetState}]. The target is the ɛ-closure of the union of ALL the
%%% NFA states reachable with that label from ANY of the states of the set.
moves(NFA, State) ->
    Pairs = [
        {L, V}
     || U <- State,
        E <- digraph:out_edges(NFA, U),
        {_, _, V, L} <- [digraph:edge(NFA, E)],
        L =/= 'ɛ'
    ],
    Labels = lists:usort([L || {L, _} <- Pairs]),
    [{Label, eps_clos(NFA, [V || {L1, V} <- Pairs, L1 =:= Label])} || Label <- Labels].

%%% The Subset Construction algorithm is used to convert a NFA to a DFA.
subset_construction(NFA) ->
    DFA = digraph:new(),
    Start = eps_clos(NFA, [1]),
    %%% Explore all the DFA states reachable from the start state
    {DFAStateL, DFATransitionsL} = explore(NFA, [Start], [Start], []),
    %%% Every DFA state (a set of NFA states) becomes a vertex of the DFA graph.
    %%% The first state is the start state, so it gets the first vertex.
    VertexEquivalenceM = convert_statel_to_graph(NFA, DFA, DFAStateL),
    %%% Every DFA transition becomes an edge of the DFA graph
    convert_transl_to_graph(DFA, VertexEquivalenceM, DFATransitionsL),
    DFA.

%%% Worklist exploration: returns {DFA states (in discovery order), DFA transitions}
explore(_NFA, [], Seen, Trans) ->
    {Seen, Trans};
explore(NFA, [S | Queue], Seen, Trans) ->
    Moves = moves(NFA, S),
    {NewSeen, NewQueue} = lists:foldl(
        fun({_Label, T}, {SeenAcc, QAcc}) ->
            case lists:member(T, SeenAcc) of
                true -> {SeenAcc, QAcc};
                false -> {SeenAcc ++ [T], QAcc ++ [T]}
            end
        end,
        {Seen, Queue},
        Moves
    ),
    NewTrans = Trans ++ [{S, T, L} || {L, T} <- Moves],
    explore(NFA, NewQueue, NewSeen, NewTrans).

%%% Convert a list of DFA state in a given DFA graph:
%%% a set of NFA states equals to a graph vertex
convert_statel_to_graph(NFA, DFA, DFAStateL) ->
    lists:foldl(
        fun(DFAState, AccM) ->
            DFAVertex = share:add_vertex(DFA),
            establish_final(NFA, DFA, DFAState, DFAVertex),
            maps:put(DFAState, DFAVertex, AccM)
        end,
        #{},
        DFAStateL
    ).

establish_final(NFA, DFA, DFAState, DFAVertex) ->
    case is_final_state(NFA, DFAState) of
        true -> set_as_final(DFA, DFAVertex);
        false -> ?UNDEFINED
    end.

%%% Convert a DFA transition list to equivalents graph edges in the DFA graph
convert_transl_to_graph(DFA, DFAVertexEquivM, DFATransitionsL) ->
    lists:foreach(
        fun({V1, V2, L}) ->
            digraph:add_edge(DFA, maps:get(V1, DFAVertexEquivM), maps:get(V2, DFAVertexEquivM), L)
        end,
        DFATransitionsL
    ).

%%% Given a graph G, this function deletes every vertex that is not reachable
%%% from the initial state (vertex 1).
remove_unreachable(G) ->
    Reachable = lists:usort([1 | digraph_utils:reachable([1], G)]),
    lists:foreach(
        fun(V) ->
            case ordsets:is_element(V, Reachable) of
                true -> ok;
                false -> digraph:del_vertex(G, V)
            end
        end,
        digraph:vertices(G)
    ).

%%% DFA minimization: given a graph G, this function will remove non-distinguishable
%%% states using the Table Filling Algorithm.
%%% The table maps every couple {V1, V2} (V1 < V2) to true if the two states are
%%% distinguishable, to false otherwise.
remove_nondistinguishable(G) ->
    Vs = lists:sort(digraph:vertices(G)),
    Table = init_scale_table(G, Vs),
    Marked = find_nondistinguishable(G, Table),
    %%% every state is replaced by the smallest state equivalent to it
    RepM = maps:from_list([{V, representative(V, Vs, Marked)} || V <- Vs]),
    merge_states(G, RepM).

%%% The smallest state equivalent to V (it is V itself if there isn't any)
representative(V, Vs, Marked) ->
    hd([W || W <- Vs, W =:= V orelse (W < V andalso not maps:get({W, V}, Marked))]).

%%% Merge every state with its representative.
merge_states(G, RepM) ->
    Drop = [V || {V, R} <- maps:to_list(RepM), V =/= R],
    %%% Edges that end in a state that is going to be deleted, and that
    %%% start from a state that is going to be kept, must be redirected.
    %%% (Edges starting from a deleted state are not needed: its
    %%% representative has equivalent outgoing edges.)
    ToRedirect = [
        {V1, maps:get(V2, RepM), L}
     || E <- digraph:edges(G),
        {_, V1, V2, L} <- [digraph:edge(G, E)],
        lists:member(V2, Drop),
        not lists:member(V1, Drop)
    ],
    lists:foreach(fun(V) -> digraph:del_vertex(G, V) end, Drop),
    lists:foreach(fun({V1, V2, L}) -> digraph:add_edge(G, V1, V2, L) end, ToRedirect).

%%% Find non-distinguishable states: repeat the marking until nothing changes
find_nondistinguishable(G, M) ->
    NewM = maps:map(
        fun
            (_Couple, true) ->
                true;
            ({S1, S2}, false) ->
                not check_if_nondistinguishable(M, G, S1, S2)
        end,
        M
    ),
    case NewM =:= M of
        true -> M;
        false -> find_nondistinguishable(G, NewM)
    end.

%%% Two states are (still) non-distinguishable if they have exactly the same
%%% labels, and for every label they go to the same state or to a couple of
%%% states that is not marked as distinguishable.
check_if_nondistinguishable(Map, G, S1, S2) ->
    T1 = get_transitions(G, S1),
    T2 = get_transitions(G, S2),
    case [L || {L, _} <- T1] =:= [L || {L, _} <- T2] of
        false ->
            false;
        true ->
            lists:all(
                fun({{_, V1}, {_, V2}}) ->
                    V1 =:= V2 orelse not maps:get(make_couple(V1, V2), Map)
                end,
                lists:zip(T1, T2)
            )
    end.

%%% Sorted list of {Label, Target} of the outgoing edges of a vertex
get_transitions(G, V) ->
    lists:sort([
        {L, T}
     || E <- digraph:out_edges(G, V),
        {_, _, T, L} <- [digraph:edge(G, E)]
    ]).

make_couple(V1, V2) when V1 < V2 -> {V1, V2};
make_couple(V1, V2) -> {V2, V1}.

%%% Initialize the Scale Table: couples of final/non-final states are already
%%% distinguishable
init_scale_table(G, Vs) ->
    maps:from_list([
        {{V1, V2}, is_final_state(G, V1) =/= is_final_state(G, V2)}
     || V1 <- Vs, V2 <- Vs, V1 < V2
    ]).

%%% If in a state's list there is at least one final state return true, otherwise false
is_final_state(G, L) when is_list(L) ->
    lists:foldl(fun(V, A) -> A or is_final_state(G, V) end, false, L);
%%% If the state is a final state return true, otherwise false
is_final_state(G, V) ->
    {V, L} = digraph:vertex(G, V),
    %%% All label's states are numbers, execept for final states
    case string:find(share:atol(L), ?FINALTAG) of
        nomatch -> false;
        _ -> true
    end.

%%% Set a state as final
set_as_final(G, V) ->
    {V, L} = digraph:vertex(G, V),
    FL = ?FINALTAG ++ integer_to_list(L),
    digraph:add_vertex(G, V, FL).

%%% Raname graph's vertex's labels in order
rename_states(G) ->
    bfs_with_raname(G, [1], [1], 1).

bfs_with_raname(G, VL, MarkedV, LastLabel) ->
    TempV = ordsets:from_list(lists:flatten([digraph:out_neighbours(G, V) || V <- VL])),
    Rename = ordsets:subtract(TempV, MarkedV),
    case Rename =:= [] of
        true ->
            done;
        false ->
            Last = lists:foldl(
                fun(I, AccI) ->
                    LNum = AccI + 1,
                    FinalL =
                        case is_final_state(G, I) of
                            true -> ?FINALTAG ++ integer_to_list(LNum);
                            false -> LNum
                        end,
                    digraph:add_vertex(G, I, FinalL),
                    LNum
                end,
                LastLabel,
                Rename
            ),
            bfs_with_raname(G, Rename, ordsets:union(MarkedV, Rename), Last)
    end.