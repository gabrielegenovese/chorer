-module(fsa).
-include("common_data.hrl").

%%% API
-export([minimize/1]).

%%%===================================================================
%%% API
%%%===================================================================

minimize(NFA) ->
    % convert a NFA to a DFA
    DFA = subset_construction(NFA),
    % Minimization of the DFA
    remove_unreachable(DFA),
    remove_nondistinguishable(DFA),
    DFA.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

eps_clos(G, VL) when is_list(VL) ->
    lists:foldl(fun(V, A) -> sets:union(eps_clos(G, V, []), A) end, sets:new(), VL).
eps_clos(G, V, ME) when not is_list(V) ->
    EL = digraph:out_edges(G, V),
    S = sets:new(),
    lists:foldl(
        fun(E, Acc) ->
            C = not lists:member(E, ME),
            case digraph:edge(G, E) of
                {E, V, V2, L} when ((L =:= 'ɛ') and (C)) ->
                    sets:union(Acc, eps_clos(G, V2, ME ++ [E]));
                _ ->
                    Acc
            end
        end,
        sets:add_element(V, S),
        EL
    ).

subset_construction(G) ->
    X = eps_clos(G, [1]),
    S = sets:new(),
    {StateL, TransitionsL} = subset_construction(
        G, sets:add_element(X, S), sets:new(), sets:new()
    ),
    DFA = digraph:new(),
    Mtemp = #{X => common_fun:add_vertex(DFA)},
    VEquivM = sets:fold(
        fun(Item, M) ->
            case Item =:= X of
                true ->
                    M;
                false ->
                    DfaV = common_fun:add_vertex(DFA),
                    NewM = maps:put(Item, DfaV, M),
                    case is_final_state(G, sets:to_list(Item)) of
                        true -> set_as_final(DFA, DfaV);
                        false -> ?UNDEFINED
                    end,
                    NewM
            end
        end,
        Mtemp,
        StateL
    ),
    sets:fold(
        fun(E, _) ->
            {V1, V2, L} = E,
            digraph:add_edge(DFA, maps:get(V1, VEquivM), maps:get(V2, VEquivM), L)
        end,
        ?UNDEFINED,
        TransitionsL
    ),
    DFA.

subset_construction(G, StateS, MarkedS, TransS) ->
    NotMarkedS = get_not_marked(StateS, MarkedS),
    case NotMarkedS =:= [] of
        true ->
            {StateS, TransS};
        false ->
            NewM = sets:add_element(NotMarkedS, MarkedS),
            {NewS, NewT} = sets:fold(
                fun(VElem, Data) ->
                    EOL = digraph:out_edges(G, VElem),
                    lists:foldl(
                        fun(E, AData) ->
                            {S, T} = AData,
                            {_, _, V, L} = digraph:edge(G, E),
                            case L of
                                'ɛ' ->
                                    AData;
                                _ ->
                                    R = eps_clos(G, [V]),
                                    Cond = sets:is_element(R, S),
                                    NeoT = sets:add_element({NotMarkedS, R, L}, T),
                                    case Cond of
                                        true -> {S, NeoT};
                                        false -> {sets:add_element(R, S), NeoT}
                                    end
                            end
                        end,
                        Data,
                        EOL
                    )
                end,
                {StateS, TransS},
                NotMarkedS
            ),
            subset_construction(G, NewS, NewM, NewT)
    end.

get_not_marked(S, Marked) ->
    Diff = sets:subtract(S, Marked),
    common_fun:first(sets:to_list(Diff)).

%%% if the number of incident edges on a vertex is 0, then delete it
remove_unreachable(G) ->
    VList = digraph:vertices(G),
    [
        digraph:del_vertex(G, V)
     || V <- VList,
        V =/= 1,
        digraph:in_degree(G, V) =:= 0
    ].

remove_nondistinguishable(G) ->
    Table = init_table(G),
    NewT = distingue_final(G, Table),
    % MinG = digraph:new(),
    M = loop_nondi(G, NewT, 0),
    maps:foreach(
        fun(K, V) ->
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
                            {_, VI, VD, L} = digraph:edge(G, E),
                            digraph:add_edge(G, VI, VnD, L),
                            digraph:del_vertex(G, VD)
                        end,
                        Ei
                    )
            end
        end,
        M
    ).

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
