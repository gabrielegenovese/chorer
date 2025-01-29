%%%-------------------------------------------------------------------
%%% @doc
%%% This module generetes the localview.
%%% Must be used after `md:extract' and `lv:generate'.
%%% @end
%%%-------------------------------------------------------------------
-module(gv).
-include("../share/common_data.hrl").

%%% API
-export([generate/0]).

%%% Record used in this module
-record(message, {from, data}).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Generate the glabal view from an entrypoint and save it in a specified folder.
generate() ->
    EntryPoint = settings:get(entrypoint),
    MainGraph = db:get_localview(EntryPoint),
    case MainGraph of
        not_found ->
            log:error("entrypoint for global view not found ~p~n", [EntryPoint]),
            error;
        _ ->
            log:info("[GV] Creating the globalview starting from ~p~n", [EntryPoint]),
            G = create_globalview(EntryPoint),
            MinG = fsa:minimize(G),
            Data = #localview{graph = G, min_graph = MinG},
            share:save_graph(Data, EntryPoint, global, settings:get(minimizeG)),
            log:info("Finished!~n", []),
            finished
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%% Create the glabal view from a function entrypoint name
create_globalview(Name) ->
    RetG = digraph:new(),
    VNew = share:add_vertex(RetG),
    N = db:inc_spawn_counter(Name),
    MainProcPid = spawn(actor_emul, proc_loop, [#actor_info{fun_name = Name, id = N}]),
    PidKey = share:atol(Name) ++ ?NSEQSEP ++ integer_to_list(N),
    ProcPidMap = #{share:ltoa(PidKey) => MainProcPid},
    S = sets:new(),
    ets:insert(?DBMANAGER, {global_state, #{1 => sets:add_element({share:ltoa(PidKey), 1}, S)}}),
    % initialize first branch
    BData = progress_all(RetG, [new_branch(RetG, VNew, ProcPidMap)]),
    % share:show_global_state(),
    BData.

new_branch(G, V, P) ->
    #branch{
        graph = G,
        last_vertex = V,
        proc_pid_m = P
    }.

new_message(F, D) ->
    #message{from = F, data = D}.

%%% Explore every possible branch of executions
progress_all(GlobalViewGraph, []) ->
    GlobalViewGraph;
progress_all(GlobalViewGraph, BranchList) when is_list(BranchList) ->
    % log:debug("Branch to eval ~p~n", [length(BranchList)]),
    NewBranchList = lists:foldl(
        fun(Item, AccList) ->
            % io:debug("Eval branch~n"),
            AccList ++ progress_branch(Item)
        end,
        [],
        BranchList
    ),
    progress_all(GlobalViewGraph, lists:flatten(NewBranchList)).

%%% Explore the execution of a single branch
progress_branch(CurrBranchData) ->
    %%% First let's eval each actor until it reaches a recv edges
    {NewBranchData, OpDone, NewBranchList} = eval_branch_until_recv(CurrBranchData),
    %%% Then, let's eval recv edges for each actor, creating a new execution branch foreach message
    RetBranchList = generate_possible_branches(NewBranchData, NewBranchList),
    %%% Return a list of possible executions or an empty list if there are none
    decide_branch_return(RetBranchList, OpDone, NewBranchData).

%%% Generate a possible execution evaluating each process
generate_possible_branches(NewBranchData, BaseBranchList) ->
    TempProcPidMap = NewBranchData#branch.proc_pid_m,
    %%% This function means we are evaluating a process that
    %%% is receiving a message before the other procesess
    maps:fold(
        fun(Name, Pid, AccList) ->
            % log:debug("[PROGSB] Name ~p MQ ~p~n", [Name, MessageQueue]),
            case actor_emul:get_proc_mess_queue(Pid) of
                ?UNDEFINED -> AccList;
                MessageQueue -> gen_branch_foreach_mess(NewBranchData, MessageQueue, Name, AccList)
            end
        end,
        BaseBranchList,
        TempProcPidMap
    ).

%%% If there are new branch to evaluate, return them. If there're not
%%% new branch but an operation has been done, return the NewBranchData.
%%% Otherwise, return an empty list; this branch is no loger evaluated.
decide_branch_return(NewBranchList, OpDone, NewBranchData) ->
    case NewBranchList =/= [] of
        true -> NewBranchList;
        false when OpDone -> [NewBranchData];
        _ -> []
    end.

%%% Generate new branches for each message accepted from an actor
gen_branch_foreach_mess(BranchData, MessageQueue, ProcName, BaseBranchList) ->
    %%% This function means we are evaluating a message
    %%% recv of a process before other messages
    lists:foldl(
        fun(Message, AccList) ->
            %%% Check if there's an edge who accepts the message
            PidMap = BranchData#branch.proc_pid_m,
            Pid = maps:get(ProcName, PidMap),
            case manage_recv(Pid, Message) of
                ?UNDEFINED -> AccList;
                %%% If an edge has been found, use the duplicated branch to add the transition to the gv
                EdgesFound -> manage_matched(BranchData, ProcName, Message, AccList, EdgesFound)
            end
        end,
        BaseBranchList,
        MessageQueue
    ).

manage_matched(BranchData, ProcName, Message, AccList, EdgesFound) ->
    lists:foldl(
        fun(EdgeFound, SecAccList) ->
            DupData = dup_branch(BranchData),
            % log:debug("[RECV1] Mess ~p Edge choose ~p~n", [Message, EdgeFound]),
            NewMap = DupData#branch.proc_pid_m,
            NewPid = maps:get(ProcName, NewMap, no_proc),
            ProcFrom = Message#message.from,
            MessData = Message#message.data,
            PidFrom = maps:get(ProcFrom, NewMap, no_proc),
            Label = format_send_label(ProcFrom, ProcName, MessData),
            % log:debug("[RECV2] LABEL ~ts~n~n", [Label]),
            ProcFromData = actor_emul:get_proc_data(PidFrom),
            case ProcFromData of
                ?UNDEFINED ->
                    log:warning("GV", "Proc data not found from", ProcFrom, SecAccList);
                _ ->
                    CurrProcFromVertex = ProcFromData#actor_info.current_state,
                    EToInfo = actor_emul:get_proc_edge_info(NewPid, EdgeFound),
                    case EToInfo of
                        false ->
                            log:warning("GV", "[MGM] Edge info not found:", EdgeFound, SecAccList);
                        ?UNDEFINED ->
                            log:warning("GV", "[MGM] Proc not found:", ProcName, SecAccList);
                        _ ->
                            actor_emul:del_proc_mess_queue(NewPid, Message),
                            {LastVertex, Added} = complex_add_vertex_recv(
                                ProcFrom, CurrProcFromVertex, ProcName, EToInfo, DupData, Label
                            ),
                            %%% NOTE: the last operation MUST be the use_proc_transition, otherwise the final graph might be wrong
                            actor_emul:use_proc_transition(NewPid, EdgeFound),
                            case Added of
                                true -> SecAccList ++ [DupData#branch{last_vertex = LastVertex}];
                                false -> SecAccList
                            end
                    end
            end
        end,
        AccList,
        EdgesFound
    ).

empty_filter_all_proc(ProcPidMap) ->
    maps:foreach(
        fun(_, Pid) ->
            actor_emul:empty_filter_proc(Pid)
        end,
        ProcPidMap
    ).

%%% Format the send label for the global view
format_send_label(ProcFrom, ProcTo, Data) ->
    share:atol(ProcFrom) ++ "→" ++ share:atol(ProcTo) ++ ":" ++ share:atol(Data).

%%% Create a duplicate for a branch object
dup_branch(Data) ->
    Data#branch{proc_pid_m = duplicate_proccess(Data#branch.proc_pid_m)}.

%%% Duplicate ideantical processes from a process' map
duplicate_proccess(ProcMap) ->
    maps:fold(
        fun(K, V, A) ->
            {Name, N} = remove_id_from_proc(K),
            NewPid = spawn(actor_emul, proc_loop, [#actor_info{fun_name = Name, id = N}]),
            case actor_emul:get_proc_data(V) of
                ?UNDEFINED ->
                    A;
                Data ->
                    actor_emul:set_proc_data(NewPid, Data),
                    maps:put(K, NewPid, A)
            end
        end,
        #{},
        ProcMap
    ).

%%% Remove the number from an actor's identificator
remove_id_from_proc(ProcId) ->
    Split = string:split(share:atol(ProcId), ?NSEQSEP),
    case Split of
        [_ | []] ->
            log:error("should have an id ~p", [ProcId]),
            {ProcId, 0};
        [Name | N] ->
            {share:ltoa(Name), N}
    end.

%%% Evaluate the edges of a local view until it reaches a receive edge foreach actor
eval_branch_until_recv(BranchData) ->
    Map = BranchData#branch.proc_pid_m,
    {NewData, OP, LL} = maps:fold(
        fun(Name, Pid, {D, B, L}) ->
            {ND, BB, LL} = eval_proc_branch(Name, Pid, D),
            {ND, B or BB, L ++ [LL]}
        end,
        {BranchData, false, []},
        Map
    ),
    case OP of
        true ->
            {ND, _, L} = eval_branch_until_recv(NewData),
            {ND, true, LL ++ L};
        false ->
            {NewData, false, LL}
    end.

%%% Evaluate the edges of a local view until it reaches a receive edge
eval_proc_branch(ProcName, ProcPid, Data) ->
    Edges = actor_emul:get_proc_edges(ProcPid),
    case Edges of
        ?UNDEFINED ->
            log:warning("GV", "[EVAL-] Process not found", ProcName, {Data, false, []});
        {Mode, EdgeList} ->
            ELLength = length(EdgeList),
            if
                (ELLength =:= 0) and (Mode =:= final_state) ->
                    check_and_set_global_final_state(Data),
                    {Data, false, []};
                (ELLength =:= 0) and (Mode =:= filtered) ->
                    % DONT REMOVE, bad idea
                    % Map = Data#branch.proc_pid_m,
                    % ProcPid ! stop,
                    % {Data#branch{proc_pid_m = maps:remove(ProcName, Map)}, false, []};
                    {Data, false, []};
                ELLength =:= 1 ->
                    E = share:first(EdgeList),
                    EI = actor_emul:get_proc_edge_info(ProcPid, E),
                    case EI of
                        ?UNDEFINED ->
                            log:warning("GV", "Process not found", ProcName, {Data, false, []});
                        _ ->
                            {D, B} = eval_edge(EI, ProcName, ProcPid, Data),
                            {D, B, []}
                    end;
                true ->
                    manage_more_edges(ProcName, ProcPid, Data)
            end
    end.

check_and_set_global_final_state(Data) ->
    IsFinal = is_global_final_state(Data),
    case IsFinal of
        true -> set_global_final_state(Data);
        false -> done
    end.

is_global_final_state(Data) ->
    maps:fold(
        fun(Name, ProcPid, Acc) ->
            case ProcPid of
                final ->
                    Acc;
                _ ->
                    Edges = actor_emul:get_proc_edges(ProcPid),
                    case Edges of
                        ?UNDEFINED ->
                            log:warning("GV", "Process not found ~p~n", [Name], Acc);
                        {Mode, _} ->
                            case Mode =/= final_state of
                                true -> false;
                                false -> Acc
                            end
                    end
            end
        end,
        true,
        Data#branch.proc_pid_m
    ).

set_global_final_state(Data) ->
    G = Data#branch.graph,
    LV = Data#branch.last_vertex,
    {LV, Label} = digraph:vertex(G, LV),
    SLabel = share:atol(Label),
    case string:find(SLabel, ?FINALTAG) of
        nomatch ->
            FinalL = ?FINALTAG ++ SLabel,
            digraph:add_vertex(G, LV, FinalL);
        _ ->
            done
    end.

%%% Evaluate edges of a localview, we are probably evaluating a receive
%%% TODO: refactor this part
%%% TODO: manage the fact that in a state there could be a recv and some other edges
manage_more_edges(ProcName, ProcPid, Data) ->
    Edges = actor_emul:get_proc_edges(ProcPid),
    case Edges of
        ?UNDEFINED ->
            log:warning("GV", "Process not found", ProcName, {Data, false, []});
        {_, EdgeList} ->
            case is_one_edgerecv(ProcPid, EdgeList) of
                true ->
                    %%% It's a receive state, block the evaluation (return false)
                    {Data, false, []};
                false ->
                    NewPossibleBranches = lists:foldl(
                        fun(SingleEdge, AccList) ->
                            DupData = dup_branch(Data),
                            NewProcPid = maps:get(ProcName, DupData#branch.proc_pid_m),
                            EdgeInfo = actor_emul:get_proc_edge_info(NewProcPid, SingleEdge),
                            case EdgeInfo of
                                ?UNDEFINED ->
                                    log:warning(
                                        "GV", "Process not found", ProcName, {Data, false, []}
                                    );
                                _ ->
                                    {EvalData, EvalDone} = eval_edge(
                                        EdgeInfo, ProcName, NewProcPid, DupData
                                    ),
                                    case EvalDone of
                                        true -> AccList ++ [EvalData];
                                        false -> AccList
                                    end
                            end
                        end,
                        [],
                        EdgeList
                    ),
                    case NewPossibleBranches =:= [] of
                        true ->
                            %%% No evaluation done, return false and old data
                            {Data, false, []};
                        false ->
                            %%% Some evaluation done, return true and new data
                            %%% Tail could be empty or a list of possible branches
                            [First | Tail] = NewPossibleBranches,
                            {First, true, Tail}
                    end
            end
    end.

%%% Given a list of edges, check if ONE is a receive edge
is_one_edgerecv(ProcPid, EL) ->
    lists:foldl(
        fun(E, A) ->
            EdgeInfo = actor_emul:get_proc_edge_info(ProcPid, E),
            case EdgeInfo of
                ?UNDEFINED -> log:warning("GV", "Process not found", ProcPid, A);
                {E, _, _, Label} -> A or is_substring(share:atol(Label), "receive")
            end
        end,
        false,
        EL
    ).

%%% Evaluate a transition from an actor
eval_edge(EdgeInfo, ProcName, ProcPid, BData) ->
    {Edge, _, _, PLabel} = EdgeInfo,
    % log:debug("Proc ~p eval label ~p~n", [ProcName, PLabel]),
    SLabel = share:atol(PLabel),
    % IsArg = is_substring(SLabel, "arg"),
    IsSpawn = is_substring(SLabel, "spawn"),
    IsSend = is_substring(SLabel, "!"),
    IsEps = is_substring(SLabel, "ɛ"),
    if
        % IsArg ->
        %     actor_emul:use_proc_transition(ProcPid, Edge),
        %     {BData, true};
        IsSpawn ->
            EdgeInfo = actor_emul:get_proc_edge_info(ProcPid, Edge),
            case EdgeInfo of
                ?UNDEFINED ->
                    log:warning("GV", "Process not found", ProcPid, {BData, false});
                _ ->
                    {VNew, NewM} = add_spawn_to_global(EdgeInfo, SLabel, ProcName, BData),
                    NewBData = BData#branch{last_vertex = VNew, proc_pid_m = NewM},
                    actor_emul:use_proc_transition(ProcPid, Edge),
                    {NewBData, true}
            end;
        IsSend ->
            manage_send(SLabel, BData, ProcName, ProcPid, Edge);
        IsEps ->
            actor_emul:use_proc_transition(ProcPid, Edge),
            {BData, true};
        true ->
            {BData, false}
    end.

%%% If SubS is substring of S return True, otherwise False.
is_substring(String, SubString) ->
    is_list(string:find(String, SubString)).

%%% Add a spawn transition to the global view
add_spawn_to_global(EInfo, SLabel, EmulProcName, Data) ->
    {_, _, PV, _} = EInfo,
    % get proc name
    FunSpawned = lists:nth(2, string:split(SLabel, " ", all)),
    {FunSName, Counter} = remove_id_from_proc(FunSpawned),
    ProcPidMap = Data#branch.proc_pid_m,
    % spawn the actor emulator
    FuncPid = spawn(actor_emul, proc_loop, [#actor_info{fun_name = FunSName, id = Counter}]),
    NewMap = maps:put(share:ltoa(FunSpawned), FuncPid, ProcPidMap),
    % get spawn argument and add them to the local variables of the actor
    LocalList = get_local_vars(EmulProcName, SLabel, FunSName),
    lists:foreach(fun(Var) -> actor_emul:add_proc_spawnvars(FuncPid, Var) end, LocalList),
    % log:debug("LocalList ~p~n", [LocalList]),
    % create the edge on the global graph
    VNew = share:add_vertex(Data#branch.graph),
    [{_, StateM}] = ets:lookup(?DBMANAGER, global_state),
    AggrGState = create_gv_state(NewMap, share:ltoa(FunSpawned), 1, EmulProcName, PV),
    % log:debug("SPAWN AGGR ~p~n", [AggrGState]),
    ets:insert(?DBMANAGER, {global_state, maps:put(VNew, AggrGState, StateM)}),
    NewLabel = format_spawn_label(SLabel, EmulProcName, FunSpawned),
    digraph:add_edge(Data#branch.graph, Data#branch.last_vertex, VNew, NewLabel),
    {VNew, NewMap}.

format_spawn_label(SLabel, EmulProcName, FunSpawned) ->
    Cond = is_list(string:find(SLabel, "args")),
    More =
        case Cond of
            true ->
                ArgsFunSpawned = lists:nth(4, string:split(SLabel, " ", all)),
                " args " ++ ArgsFunSpawned;
            false ->
                ""
        end,
    share:atol(EmulProcName) ++ "Δ" ++ FunSpawned ++ More.

%%% TODO: refactor
get_local_vars(ProcId, Label, FunSName) ->
    EM = db:get_lv_edge_additonal_info(element(1, remove_id_from_proc(ProcId))),
    InputData = maps:get(Label, EM, []),
    % log:debug("[GV] EmulProcName ~p Label ~p Input ~p~n", [FunSName, Label, EM]),
    % add input data to local vars
    case InputData of
        [] ->
            [];
        _ ->
            [{_, Input}] = ets:lookup(?ARGUMENTS, share:atol(FunSName)),
            % log:debug("[GV] for fun ~p found ~p~n", [share:atol(FunSName), Input]),
            {LL, Remain} = lists:foldl(
                fun({var, _, Name}, {A, In}) ->
                    case In of
                        [] ->
                            log:error("list should NOT be empty but there is ~p~n", Name),
                            {A ++ [#variable{name = Name}], []};
                        [H | T] ->
                            case H#variable.value of
                                "pid_self" ->
                                    {A ++ [#variable{type = pid, name = Name, value = ProcId}], T};
                                _ ->
                                    {A ++ [H#variable{name = Name}], T}
                            end
                    end
                end,
                {[], InputData#variable.value},
                Input
            ),
            case Remain =:= [] of
                true -> done;
                false -> log:error("list should be empty but there is ~p~n", Remain)
            end,
            LL
    end.

%%% Evaluate a send transition of an actor
manage_send(SendLabel, Data, ProcName, ProcPid, Edge) ->
    ProcPidMap = Data#branch.proc_pid_m,
    DataSent = get_data_from_send_label(SendLabel),
    ProcSentTemp = share:ltoa(get_proc_from_send_label(SendLabel)),
    IsVar = share:is_erlang_variable(ProcSentTemp),
    ProcSentName =
        case IsVar of
            true -> check_vars(ProcPid, ProcSentTemp);
            false -> share:ltoa(check_pid_self(ProcSentTemp, ProcName))
        end,
    ProcSentPid = maps:get(share:ltoa(ProcSentName), ProcPidMap, no_pid),
    case ProcSentPid of
        no_pid ->
            log:warning("GV", "no pid found, var/pid", [ProcSentTemp, ProcSentName], done),
            check_send_to_not_existing_proc(ProcSentName, ProcName),
            {Data, false};
        P ->
            actor_emul:add_proc_mess_queue(P, new_message(ProcName, DataSent)),
            %%% NOTE: the last operation MUST be use_proc_transition, otherwise the final graph might be wrong
            actor_emul:use_proc_transition(ProcPid, Edge),
            {Data, true}
    end.

check_send_to_not_existing_proc(Proc, ProcName) ->
    SProc = share:atol(Proc),
    Check = lists:member(?ARITYSEP, SProc),
    S1 = string:split(SProc, ?ARITYSEP),
    case S1 of
        [_Name | T] when Check ->
            % log:debug("[CHECK] Name ~p~n", [Name]),
            S2 = string:split(share:atol(T), ?NSEQSEP),
            case S2 of
                [_Arity | _Seq] ->
                    log:warning(
                        "GV",
                        "[WARNING] ~p is a process ID but the process didn't start yet.~n" ++
                            "The process ~p should start before ~p.~n",
                        [Proc, Proc, ProcName],
                        done
                    );
                _ ->
                    done
            end;
        _ ->
            done
    end.

%%% Evaluate a receive transition of an actor
%%% TODO: refactor
manage_recv(ProcPid, Message) ->
    Edges = actor_emul:get_proc_edges(ProcPid),
    case Edges of
        ?UNDEFINED ->
            log:warning("GV", "Process not found", ProcPid, ?UNDEFINED);
        {_, EdgeList} ->
            %%% TODO: change to all when false branch is ready
            IsRecvList = is_one_edgerecv(ProcPid, EdgeList),
            % log:debug("IsRECV ~p EL ~p~n", [IsRecv, EL]),
            case IsRecvList of
                false ->
                    % TODO: manage when is not ONLY a receive edge, like:
                    %         3
                    %        /recv
                    % 1 -> 2
                    %        \send
                    %         4
                    %
                    % The following code is old, CHECK before uncommenting
                    % {NewL, NOp} = lists:foldl(
                    %     fun(E, A) ->
                    %         EInfo = get_proc_edge_info(ProcPid, E),
                    %         {B, C} = A,
                    %         {D, O} = eval_edge(EInfo, ProcName, ProcPid, Data),
                    %         {B ++ [D], C or O}
                    %     end,
                    %     {[], false},
                    %     EL
                    % ),
                    % [H | T] = NewL,
                    % {H, T, NOp};
                    ?UNDEFINED;
                true ->
                    OrderedEdges = custom_sort_edges(ProcPid, EdgeList),
                    case is_any(Message) of
                        %   over-approximation: select all edges that match
                        true -> OrderedEdges;
                        false -> check_compatibility_with_stop(ProcPid, OrderedEdges, Message)
                    end
            end
    end.

check_compatibility_with_stop(ProcPid, OrderedEdges, Message) ->
    From = Message#message.from,
    {_, EdgeChoosen} = lists:foldl(
        fun(Edge, {AlreadyFound, RetEdge}) ->
            case AlreadyFound of
                true ->
                    {AlreadyFound, RetEdge};
                false ->
                    EdgeInfo = actor_emul:get_proc_edge_info(ProcPid, Edge),
                    case EdgeInfo of
                        ?UNDEFINED ->
                            log:warning(
                                "GV", "Process not found", ProcPid, RetEdge
                            );
                        {_, _, _, ELabel} ->
                            IsCompatible = is_pm_msg_compatible(ProcPid, From, ELabel, Message),
                            case IsCompatible of
                                true -> {true, Edge};
                                false -> {AlreadyFound, RetEdge}
                            end
                    end
            end
        end,
        {false, ?UNDEFINED},
        OrderedEdges
    ),
    [EdgeChoosen].

custom_sort_edges(ProcPid, EL) ->
    SepE =
        lists:foldl(
            fun(E, L) ->
                EdgeInfo = actor_emul:get_proc_edge_info(ProcPid, E),
                case EdgeInfo of
                    ?UNDEFINED ->
                        log:warning("GV", "Process not found", ProcPid, L);
                    {_, _, _, ELabel} ->
                        Label = share:atol(ELabel),
                        Cond = is_list(string:find(Label, "receive")),
                        case Cond of
                            true ->
                                [N, P] = string:split(share:atol(ELabel), ?PMSEQSEP),
                                L ++ [{list_to_integer(N), P, E}];
                            false ->
                                L
                        end
                end
            end,
            [],
            EL
        ),
    Sort = lists:sort(fun({N1, _, _}, {N2, _, _}) -> N1 < N2 end, SepE),
    % log:debug("sorted ~p~n", [Sort]),
    [E || {_, _, E} <- Sort].

%%% Find the actor id from a variable's list, given the variable name
check_vars(ProcPid, VarName) ->
    ProcLocalVars = actor_emul:get_proc_localvars(ProcPid),
    case ProcLocalVars of
        ?UNDEFINED ->
            VarName;
        _ ->
            % log:debug("Find var ~p in ~p pid ~p~n", [VarName, ProcLocalVars, ProcPid]),
            VarValue = share:find_var(ProcLocalVars, VarName),
            % log:debug("Found ~p~n", [VarValue]),
            case VarValue of
                not_found ->
                    VarName;
                V ->
                    % log:debug("Found ~p~n", [V#variable.type]),
                    case V#variable.type of
                        "pid" -> V#variable.value;
                        pid -> V#variable.value;
                        ?UNDEFINED -> VarName;
                        _ -> V#variable.type
                    end
            end
    end.

%%% Remove the "pid_" part from a variable's type
% remove_pid_part(Data) -> ltoa(lists:flatten(string:replace(atol(Data), "pid_", ""))).

%%% Check if a pattern metching match a message, then register the new variables
is_pm_msg_compatible(ProcPid, CallingProc, PatternMatching, Message) ->
    %% removing receive
    [_ | PM] = string:split(share:atol(PatternMatching), "receive "),
    PatternMS = lists:flatten(PM),
    %% check pm
    {IsCompatible, ToRegisterList} = check_msg_comp(
        ProcPid, CallingProc, PatternMS, Message#message.data
    ),
    % log:debug("Reg List ~p~n", [ToRegisterList]),
    lists:foreach(
        fun(Item) -> register_var(Item) end,
        ToRegisterList
    ),
    IsCompatible.

%%% Check if a pattern metching match a message
check_msg_comp(ProcPid, CallingProc, PatternMatching, Message) ->
    MessageS = share:atol(Message),
    [FirstPtmtChar | RestPtmt] = PatternMatching,
    [FirstMessChar | RestMess] = MessageS,
    IsFirstCharUpperCase = share:is_erlang_variable(PatternMatching),
    AnyS = share:atol(?ANYDATA),
    if
        % over-approximation
        MessageS =:= AnyS ->
            {true, []};
        ([FirstPtmtChar] =:= "{") and ([FirstMessChar] =:= "{") ->
            check_tuple(ProcPid, CallingProc, RestPtmt, RestMess);
        PatternMatching =:= MessageS ->
            {true, []};
        IsFirstCharUpperCase ->
            {true, [{ProcPid, share:ltoa(PatternMatching), check_pid_self(Message, CallingProc)}]};
        [FirstPtmtChar] =:= "_" ->
            {true, []};
        true ->
            {false, []}
    end.

is_any(M) ->
    AnyS = share:atol(?ANYDATA),
    MessS = share:atol(M#message.data),
    MessS =:= AnyS.

%%% AGGIORNARE COMPATIBILITA':
%% se sono sicuro che fa match, genero e non vado a vedere altro
%% se sicuro che non fa match, non genero e vado a vedere il prossimo arco
%% non sono sicuro, genero e vado a vedere il prossimo

%%% più avanti, match coi tipi
%%% fare esempi con X + 1, oppure X = io:fread, Pid ! X

%%% Check if the rest of the tuple is compatible
check_tuple(ProcPid, CallingProc, RestPtmt, RestMess) ->
    ContentPtmt = share:remove_last(RestPtmt),
    ContentMess = share:remove_last(RestMess),
    ContentPtmtList = string:split(ContentPtmt, ",", all),
    EnumPtmt = lists:enumerate(ContentPtmtList),
    ContentMessList = string:split(ContentMess, ",", all),
    EnumMess = lists:enumerate(ContentMessList),
    BoolList = [
        check_msg_comp(ProcPid, CallingProc, DataPtmt, DataMess)
     || {IndexPtmt, DataPtmt} <- EnumPtmt,
        {IndexMess, DataMess} <- EnumMess,
        IndexPtmt =:= IndexMess
    ],
    share:and_rec(BoolList).

%%% Register a actor's variable
register_var(Data) ->
    {ProcPid, Name, Value} = Data,
    %%% type = pid to change, for now it's ok like this because I only focus on pid exchange
    V = #variable{name = share:ltoa(Name), type = pid, value = share:ltoa(Value)},
    % log:debug("Added Var ~p~n", [V]),
    actor_emul:add_proc_localvars(ProcPid, V).

%%% Substitute pif_self to pid_ProcId
check_pid_self(Data, ProcId) ->
    % log:debug("[C]Data ~p proc id ~p~n", [Data, ProcId]),
    lists:flatten(string:replace(share:atol(Data), "pid_self", share:atol(ProcId))).

%%% Add a vertex to the global view, with some checks for recusive edges

%%% Check if a equal global state exist, otherwise add a new one.
%%% TODO: consider message queue also
complex_add_vertex_recv(Proc1, CurrVertex, Proc2, EdgeInfo, Data, Label) ->
    ProcPid = Data#branch.proc_pid_m,
    [{_, StateM}] = ets:lookup(?DBMANAGER, global_state),
    % log:debug("stateM ~p~n", [StateM]),
    % log:debug("Label ~p~n", [share:ltoa(Label)]),
    VLast = Data#branch.last_vertex,
    G = Data#branch.graph,
    {_, _PV1, PV2, _} = EdgeInfo,
    EL = digraph:out_edges(G, VLast),
    SameL = check_same_label(G, EL, VLast, Label),
    % log:debug("~n~n[COMPLEX] new check~n", []),
    AggregateGlobalState = create_gv_state(ProcPid, Proc1, CurrVertex, Proc2, PV2),
    case check_if_exist(StateM, AggregateGlobalState) of
        nomatch ->
            VNew = share:add_vertex(G),
            digraph:add_edge(G, VLast, VNew, Label),
            % log:debug("[DEBUG] Adding new global state ~p~n", [VNew]),
            NewM = maps:put(VNew, AggregateGlobalState, StateM),
            ets:insert(?DBMANAGER, {global_state, NewM}),
            empty_filter_all_proc(Data#branch.proc_pid_m),
            {VNew, true};
        VFound ->
            % log:debug("[EXTERNFOUND] ~p~n", [VFound]),
            case SameL of
                nomatch ->
                    digraph:add_edge(G, VLast, VFound, Label),
                    {VFound, true};
                VTo ->
                    {VTo, false}
            end
    end.

%%% Creation of a new state for the global view
%%% A global state is the set of the states of all the processes.
%%% The state of a processes is a triple with:
%%% - its name;
%%% - its state;
%%% - its message queue.
create_gv_state(ProcPid, Proc1, V2, Proc2, PV2) ->
    maps:fold(
        fun(Name, Pid, AccList) ->
            RealName = share:remove_counter(Name),
            LV = db:get_localview(RealName),
            Graph = LV#localview.graph,
            case actor_emul:get_proc_data(Pid) of
                ?UNDEFINED ->
                    AccList;
                Data ->
                    MessageQueue = Data#actor_info.message_queue,
                    EL =
                        case Name of
                            Proc1 ->
                                {_, L} = digraph:vertex(Graph, V2),
                                % {Proc1, share:if_final_get_n(L)};
                                {Proc1, share:if_final_get_n(L), sets:from_list(MessageQueue)};
                            Proc2 ->
                                {_, L} = digraph:vertex(Graph, PV2),
                                % {Proc2, share:if_final_get_n(L)};
                                {Proc1, share:if_final_get_n(L), sets:from_list(MessageQueue)};
                            N ->
                                {_, L} = digraph:vertex(Graph, Data#actor_info.current_state),
                                % {N, share:if_final_get_n(L)}
                                {N, share:if_final_get_n(L), sets:from_list(MessageQueue)}
                        end,
                    sets:add_element(EL, AccList)
            end
        end,
        sets:new(),
        ProcPid
    ).

check_if_exist(StateM, AggregateGlobalState) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            if
                Acc =:= nomatch ->
                    %%% Check if the set has the same elements
                    Cond = Value =:= AggregateGlobalState,
                    % log:debug("[SUB] ~p~n", [Cond]),
                    case Cond of
                        true ->
                            % log:debug("[FOUND] FOUND n ~p ~n", [Key]),
                            Key;
                        false ->
                            Acc
                    end;
                true ->
                    Acc
            end
        end,
        nomatch,
        StateM
    ).

%%% Returns the outgoing vertex given a label and a list of transition
check_same_label(G, EL, VLast, Label) ->
    lists:foldl(
        fun(E, A) ->
            {E, VFrom, VTo, VLabel} = digraph:edge(G, E),
            case (VLast =:= VFrom) and (VLabel =:= Label) of
                true when A =:= nomatch -> VTo;
                false -> A;
                _ -> A
            end
        end,
        nomatch,
        EL
    ).

%%% Get the data from a send local view's label
get_data_from_send_label(Label) ->
    string:trim(lists:nth(2, string:split(Label, ?SENDSEP))).

%%% Get the process from a send local view's label
get_proc_from_send_label(Label) ->
    string:trim(lists:nth(1, string:split(Label, ?SENDSEP))).

%%% Stop all the processes from the process map
% stop_processes(DataL) when is_list(DataL) ->
%     lists:foreach(
%         fun(D) ->
%             M = D#branch.proc_pid_m,
%             stop_processes(M)
%         end,
%         DataL
%     );
% stop_processes(ProcMap) ->
%     maps:foreach(fun(_K, V) -> V ! stop end, ProcMap).
