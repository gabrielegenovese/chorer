-module(fsa).
-include("common_data.hrl").

%%% API
-export([minimize/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%% Minimize an FSA, using algorithms from Simone Martini and Maurizio Gabbrielli's
%%% book: Programming Languages, Second edition, McGraw-Hill, 2011
minimize(NFA) ->
    % Convert a NFA to a DFA
    DFA = subset_construction(NFA),
    % Minimization of the DFA
    remove_unreachable(DFA),
    remove_nondistinguishable(DFA),
    rename_states(DFA),
    DFA.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%% Given a list of state, it returns all the states reachable with an ɛ transition
eps_clos(Graph, VertexL) when is_list(VertexL) ->
    lists:foldl(
        fun(Vertex, AccS) ->
            NewSet = eps_clos(Graph, Vertex, []),
            %%% Return the union between the new set and the previous set
            sets:union(NewSet, AccS)
        end,
        sets:new(),
        VertexL
    ).

%%% Given a state, it returns all the states reachable with an ɛ transition
eps_clos(Graph, Vertex, MarkedEdges) when not is_list(Vertex) ->
    %%% Get outer transitions
    OutTransitions = digraph:out_edges(Graph, Vertex),
    %%% Create a set with the base vertex
    RetS = sfroml([Vertex]),
    %%% For all transition check if marked and Label == ɛ
    FoldlFun = fun(Edge, AccS) ->
        %%% Marking states is used to avoid loopss
        Cond = not lists:member(Edge, MarkedEdges),
        case digraph:edge(Graph, Edge) of
            {Edge, Vertex, V2, Label} when ((Label =:= 'ɛ') and (Cond)) ->
                NewS = eps_clos(Graph, V2, MarkedEdges ++ [Edge]),
                sets:union(AccS, NewS);
            _ ->
                AccS
        end
    end,
    lists:foldl(FoldlFun, RetS, OutTransitions).

%%% The Subset Construction algorith is used to convert a NFA to a DFA.
subset_construction(NFA) ->
    DFA = digraph:new(),
    FirstDFAState = eps_clos(NFA, [1]),
    %%% Create a list of DFA states and transitions
    {DFAStateL, DFATransitionsL} = subset_construction(NFA, sfroml([FirstDFAState])),
    %%% Convert the list of DFA state in the DFA graph vertex
    VertexEquivalanceM = convert_statel_to_graph(NFA, DFA, FirstDFAState, DFAStateL),
    %%% Convert the list of DFA transition in the DFA graph edges
    convert_transl_to_graph(DFA, VertexEquivalanceM, DFATransitionsL),
    DFA.

%%% Initialize new sets for subset_construction/4
subset_construction(NFA, StateS) ->
    subset_construction(NFA, StateS, sets:new(), sets:new()).

%%% Create recursively a list of DFA states and transactions, given an NFA graph
subset_construction(NFA, StateS, TransS, MarkedS) ->
    NotMarkedS = get_not_marked(StateS, MarkedS),
    case NotMarkedS =:= [] of
        true ->
            {StateS, TransS};
        false ->
            NewMarkedList = sets:add_element(NotMarkedS, MarkedS),
            {NewS, NewT} = sets:fold(
                fun(VElem, Data) ->
                    EOL = digraph:out_edges(NFA, VElem),
                    LFoldlFun = fun(E, AData) ->
                        {S, T} = AData,
                        {_, _, V, Label} = digraph:edge(NFA, E),
                        case Label of
                            'ɛ' ->
                                AData;
                            _ ->
                                R = eps_clos(NFA, [V]),
                                Cond = sets:is_element(R, S),
                                NeoT = sets:add_element({NotMarkedS, R, Label}, T),
                                case Cond of
                                    true -> {S, NeoT};
                                    false -> {sets:add_element(R, S), NeoT}
                                end
                        end
                    end,
                    lists:foldl(LFoldlFun, Data, EOL)
                end,
                {StateS, TransS},
                NotMarkedS
            ),
            subset_construction(NFA, NewS, NewT, NewMarkedList)
    end.

%%% Convert a list of DFA state in a given DFA graph:
%%% a set of NFA states equals to a graph vertex
convert_statel_to_graph(NFA, DFA, FirstDFAState, DFAStateL) ->
    RetM = #{FirstDFAState => common_fun:add_vertex(DFA)},
    FoldFun = fun(DFAState, AccM) ->
        case DFAState =:= FirstDFAState of
            true ->
                AccM;
            false ->
                DFAVertex = common_fun:add_vertex(DFA),
                case is_final_state(NFA, sets:to_list(DFAState)) of
                    true -> set_as_final(DFA, DFAVertex);
                    false -> ?UNDEFINED
                end,
                maps:put(DFAState, DFAVertex, AccM)
        end
    end,
    sets:fold(FoldFun, RetM, DFAStateL).

%%% Convert a DFA transition list to equivalents graph edges in the DFA graph
convert_transl_to_graph(DFA, DFAVertexEquivM, DFATransitionsL) ->
    lists:foreach(
        fun(DFATransition) ->
            {V1, V2, L} = DFATransition,
            digraph:add_edge(DFA, maps:get(V1, DFAVertexEquivM), maps:get(V2, DFAVertexEquivM), L)
        end,
        stol(DFATransitionsL)
    ).

%%% Returns an unmarked state if present, otherwise returns an empty list
get_not_marked(AllStateS, MarkedStateS) ->
    DiffS = sets:subtract(AllStateS, MarkedStateS),
    common_fun:first(stol(DiffS)).

%%% Given a graph G, this function will delete every unreachable vertex, that is
%%% if the number of incident edges on a vertex is 0, then delete it
remove_unreachable(G) ->
    VertexList = digraph:vertices(G),
    [
        digraph:del_vertex(G, V)
     || V <- VertexList,
        V =/= 1,
        digraph:in_degree(G, V) =:= 0
    ].

%%% DFA minimization: given a graph G, this function will remove non-distinguishable
%%% states using the Table Filling at Scale Algorithm
remove_nondistinguishable(G) ->
    Table = init_scale_table(G),
    InitMarkedStates = distinguish_final(G, Table),
    FinalMarkedStatesMap = find_nondistinguishable(G, InitMarkedStates),
    ForEachFun = fun(K, V) ->
        case V of
            true ->
                done;
            false ->
                {V1, V2} = get_both_states(K),
                {VD, VnD} =
                    case V1 > V2 of
                        true -> {V1, V2};
                        false -> {V2, V1}
                    end,
                Ei = digraph:in_edges(G, VD),
                lists:foreach(
                    fun(E) ->
                        case digraph:edge(G, E) of
                            false -> ?UNDEFINED;
                            {E, VI, VD, L} -> digraph:add_edge(G, VI, VnD, L)
                        end
                    end,
                    Ei
                ),
                digraph:del_vertex(G, VD)
        end
    end,
    maps:foreach(ForEachFun, FinalMarkedStatesMap).

%%% Find non-distinguishable states: a state is non-distinguishable if given two state
%%% and a path to follow, they end up in the same state for all their transitions
find_nondistinguishable(G, M) ->
    {OpDone, NewL} = maps:fold(
        fun(StateCouple, IsMarked, A) ->
            {_, UpdatedM} = A,
            {S1, S2} = get_both_states(StateCouple),
            case IsMarked of
                true ->
                    A;
                false ->
                    B = check_if_nondistinguishable(UpdatedM, G, S1, S2),
                    case B of
                        false -> {true, maps:put(StateCouple, true, UpdatedM)};
                        true -> A
                    end
            end
        end,
        {false, M},
        M
    ),
    case OpDone of
        true -> find_nondistinguishable(G, NewL);
        false -> M
    end.

%%% Check if two states non-distinguishable
check_if_nondistinguishable(Map, G, S1, S2) ->
    L1 = get_all_labels(G, S1),
    L2 = get_all_labels(G, S2),
    LL =
        if
            length(L1) >= length(L2) -> L1;
            true -> L2
        end,
    lists:foldl(
        fun(L, A) ->
            V1 = get_vertex_using_label(G, S1, L),
            case get_vertex_using_label(G, S1, L) of
                false ->
                    false;
                V1 ->
                    case get_vertex_using_label(G, S2, L) of
                        false ->
                            false;
                        V2 ->
                            SKey = sets:add_element(V2, sets:add_element(V1, sets:new())),
                            A and
                                case V1 =/= V2 of
                                    true -> maps:get(SKey, Map);
                                    false -> true
                                end
                    end
            end
        end,
        true,
        LL
    ).

%%% Given a vertex, returns each vertex than is reached using a given label
get_vertex_using_label(Graph, Vertex, Label) ->
    EL = digraph:out_edges(Graph, Vertex),
    lists:foldl(
        fun(E, A) ->
            case digraph:edge(Graph, E) of
                {_, _, VRet, Label} -> VRet;
                _ -> A
            end
        end,
        false,
        EL
    ).

%%% Get the list of all label's vertex
get_all_labels(G, V) ->
    EL = digraph:out_edges(G, V),
    stol(
        lists:foldl(
            fun(E, A) ->
                case digraph:edge(G, E) of
                    {_, _, _, L} -> sets:add_element(L, A);
                    _ -> A
                end
            end,
            sets:new(),
            EL
        )
    ).

%%% Initialize the Scale Table
init_scale_table(Graph) ->
    VertexList = digraph:vertices(Graph),
    %% double convertion to remove equals couples
    stol(
        sfroml([
            sets:add_element(V1, sets:add_element(V2, sets:new()))
         || V1 <- VertexList, V2 <- VertexList, V1 =/= V2
        ])
    ).

%%% Distinguish final and non-final states
distinguish_final(G, Table) ->
    lists:foldl(
        fun(I, Acc) ->
            {H, T} = get_both_states(I),
            IsHF = is_final_state(G, H),
            IsTF = is_final_state(G, T),
            IMark = ((not IsHF and IsTF) or (IsHF and not IsTF)),
            maps:merge(Acc, #{I => IMark})
        end,
        #{},
        Table
    ).

%%% Format a list of two element into a couple
get_both_states(S) ->
    L = stol(S),
    [H | [T | _]] = L,
    {H, T}.

%%% If in a state's list there are all final state return true, otherwise false
is_final_state(G, L) when is_list(L) ->
    lists:foldl(fun(V, A) -> A or is_final_state(G, V) end, false, L);
%%% If the state is a final state return true, otherwise false
is_final_state(G, V) ->
    {V, L} = digraph:vertex(G, V),
    %%% All label's states are numbers, execept for final states
    not is_integer(L).

%%% Set a state as final
set_as_final(G, V) ->
    {V, L} = digraph:vertex(G, V),
    FL = ?FINALTAG ++ integer_to_list(L),
    digraph:add_vertex(G, V, FL).

stol(S) -> sets:to_list(S).
sfroml(S) -> sets:from_list(S).

%%% Raname graph's vertex's labels in order
rename_states(G) ->
    bfs_with_raname(G, [1], sfroml([1]), 1).

bfs_with_raname(G, VL, MarkedV, LastLabel) ->
    TempV = sfroml(lists:flatten([digraph:out_neighbours(G, V) || V <- VL])),
    % io:fwrite("NEIGHBOURS LIST ~p~n", [stol(TempV)]),
    Rename = stol(sets:subtract(TempV, MarkedV)),
    % io:fwrite("RENAME LIST ~p~n", [Rename]),
    case Rename =:= [] of
        true ->
            done;
        false ->
            Last = lists:foldl(
                fun(I, AccI) ->
                    LNum = AccI + 1,
                    C = is_final_state(G, I),
                    FinalL =
                        case C of
                            true -> ?FINALTAG ++ integer_to_list(LNum);
                            false -> LNum
                        end,
                    digraph:add_vertex(G, I, FinalL),
                    LNum
                end,
                LastLabel,
                Rename
            ),
            bfs_with_raname(G, Rename, sets:union(MarkedV, sfroml(Rename)), Last)
    end.
