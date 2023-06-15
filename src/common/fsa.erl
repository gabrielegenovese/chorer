-module(fsa).
-include("common_data.hrl").

%%% API
-export([minimize/1]).

%%%===================================================================
%%% API
%%%===================================================================

minimize(NFA) ->
    % Convert a NFA to a DFA
    DFA = subset_construction(NFA),
    % Minimization of the DFA
    remove_unreachable(DFA),
    remove_nondistinguishable(DFA),
    DFA.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%% Given a list of state, it returns all the states reachable with an ɛ transition.
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

%%% Given a state, it returns all the states reachable with an ɛ transition.
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

%%% This function initialize new sets for subset_construction/4
subset_construction(NFA, StateS) ->
    subset_construction(NFA, StateS, sets:new(), sets:new()).

%%% This recursive function create a list of DFA states and transactions, given an NFA graph
subset_construction(NFA, StateS, TransS, MarkedS) ->
    NotMarkedS = get_not_marked(StateS, MarkedS),
    case NotMarkedS =:= [] of
        true ->
            {StateS, TransS};
        false ->
            NewM = sets:add_element(NotMarkedS, MarkedS),
            {NewS, NewT} = sets:fold(
                fun(VElem, Data) ->
                    EOL = digraph:out_edges(NFA, VElem),
                    LFoldlFun = fun(E, AData) ->
                        {S, T} = AData,
                        {_, _, V, L} = digraph:edge(NFA, E),
                        case L of
                            'ɛ' ->
                                AData;
                            _ ->
                                R = eps_clos(NFA, [V]),
                                Cond = sets:is_element(R, S),
                                NeoT = sets:add_element({NotMarkedS, R, L}, T),
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
            subset_construction(NFA, NewS, NewT, NewM)
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
                NewM = maps:put(DFAState, DFAVertex, AccM),
                case is_final_state(NFA, sets:to_list(DFAState)) of
                    true -> set_as_final(DFA, DFAVertex);
                    false -> ?UNDEFINED
                end,
                NewM
        end
    end,
    sets:fold(FoldFun, RetM, DFAStateL).

%%% Covert a DFA transition list to equivalents graph edges in the DFA graph
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

%%% DFA minimization: given a graph G, this function will remove non-distinguishable states
remove_nondistinguishable(G) ->
    Table = init_table(G),
    NewT = distingue_final(G, Table),
    M = loop_nondi(G, NewT, 0),
    ForEachFun = fun(K, V) ->
        case V of
            true ->
                done;
            false ->
                {V1, V2} = get_both(K),
                {VD, VnD} =
                    case V1 > V2 of
                        true -> {V1, V2};
                        false -> {V2, V1}
                    end,
                Ei = digraph:in_edges(G, VD),
                lists:foreach(
                    fun(E) ->
                        case digraph:edge(G, E) of
                            false ->
                                ?UNDEFINED;
                            {E, VI, VD, L} ->
                                digraph:add_edge(G, VI, VnD, L),
                                digraph:del_vertex(G, VD)
                        end
                    end,
                    Ei
                )
        end
    end,
    maps:foreach(ForEachFun, M).

loop_nondi(G, M, Jump) ->
    {OpDone, NewL} = maps:fold(
        fun(StateCouple, IsMarked, A) ->
            {_, UpdatedM} = A,
            {S1, S2} = get_both(StateCouple),
            case IsMarked of
                true ->
                    A;
                false ->
                    B = check_if_eq(UpdatedM, G, S1, S2, Jump),
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
        true -> loop_nondi(G, NewL, Jump + 1);
        false -> M
    end.

check_if_eq(UpdatedM, G, S1, S2, _Jump) ->
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
                                    true -> maps:get(SKey, UpdatedM);
                                    false -> true
                                end
                    end
            end
        end,
        true,
        LL
    ).

get_vertex_using_label(G, V, L) ->
    EL = digraph:out_edges(G, V),
    lists:foldl(
        fun(E, A) ->
            case digraph:edge(G, E) of
                {_, _, VO, L} -> VO;
                _ -> A
            end
        end,
        false,
        EL
    ).

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

init_table(G) ->
    VL = digraph:vertices(G),
    stol(
        sfroml([
            sets:add_element(V1, sets:add_element(V2, sets:new()))
         || V1 <- VL, V2 <- VL, V1 =/= V2
        ])
    ).

distingue_final(G, Table) ->
    lists:foldl(
        fun(I, Acc) ->
            {H, T} = get_both(I),
            IsHF = is_final_state(G, H),
            IsTF = is_final_state(G, T),
            NewM = ((not IsHF and IsTF) or (IsHF and not IsTF)),
            maps:merge(Acc, #{I => NewM})
        end,
        #{},
        Table
    ).

get_both(S) ->
    L = stol(S),
    [H | [T | _]] = L,
    {H, T}.

is_final_state(G, L) when is_list(L) ->
    lists:foldl(fun(V, A) -> A or is_final_state(G, V) end, false, L);
is_final_state(G, V) ->
    {V, L} = digraph:vertex(G, V),
    not is_integer(L).

set_as_final(G, V) ->
    {V, L} = digraph:vertex(G, V),
    FL = ?FINALTAG ++ integer_to_list(L),
    digraph:add_vertex(G, V, FL).

stol(S) -> sets:to_list(S).
sfroml(S) -> sets:from_list(S).
