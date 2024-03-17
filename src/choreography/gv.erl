%%%-------------------------------------------------------------------
%%% @doc
%%% This module generetes the localview.
%%% Must be used after `md:extract' and `lv:generate'.
%%% @end
%%%-------------------------------------------------------------------
-module(gv).
-include("../share/common_data.hrl").

%%% API
-export([generate/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Generate the glabal view from an entrypoint and save it in a specified folder.
generate(Settings, EntryPoint) ->
    MainGraph = share:get_localview(EntryPoint),
    case MainGraph of
        not_found ->
            io:fwrite("Error: entrypoint for global view not found ~p~n", [EntryPoint]),
            error;
        _ ->
            % io:fwrite("Creating the globalview starting from ~p~n", [EntryPoint]),
            G = create_globalview(EntryPoint),
            MinG = fsa:minimize(G),
            Data = #wip_lv{graph = G, min_graph = MinG},
            share:save_graph(Data, Settings, EntryPoint, global),
            finished
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%% Create the glabal view from a function entrypoint name
create_globalview(Name) ->
    RetG = digraph:new(),
    VNew = share:add_vertex(RetG),
    N = share:inc_spawn_counter(Name),
    MainProcPid = spawn(actor_emul, proc_loop, [#actor_info{fun_name = Name, id = N}]),
    PidKey = share:atol(Name) ++ ?SEPARATOR ++ integer_to_list(N),
    ProcPidMap = #{share:ltoa(PidKey) => MainProcPid},
    % initialize first branch
    progress_procs(RetG, [new_branch(RetG, VNew, ProcPidMap)]).

new_branch(G, V, P) ->
    #branch{
        graph = G,
        last_vertex = V,
        proc_pid_m = P
    }.

new_message(F, D, E) ->
    #message{
        from = F, data = D, edge = E
    }.

%%% Explore every possible branch of executions
progress_procs(G, []) ->
    G;
progress_procs(GlobalGraph, BranchList) when is_list(BranchList) ->
    RealList = lists:flatten(BranchList),
    % io:fwrite("Branch to eval ~p~n", [length(RealList)]),
    NewBL = lists:foldl(
        fun(Item, AccL) ->
            % io:fwrite("Eval branch~n"),
            NewBreanches = progress_single_branch(Item),
            AccL ++ NewBreanches
        end,
        [],
        RealList
    ),
    progress_procs(GlobalGraph, NewBL).

%%% Explore the execution of a single branch
progress_single_branch(BData) ->
    %%% First let's eval each actor until it reaches a recv edges
    {TempBranchData, OpDone, NBL} = eval_branch_until_recv(BData),
    %%% Then, let's eval recv edges for each actor, creating a new execution branch foreach message
    TempProcPidMap = TempBranchData#branch.proc_pid_m,
    NewBranchList =
        NBL ++
            maps:fold(
                fun(Name, Pid, AccList) ->
                    MessageQueue = actor_emul:get_proc_mess_queue(Pid),
                    % io:fwrite("[PROGSB] Name ~p MQ ~p~n", [Name, MessageQueue]),
                    gen_branch_foreach_mess(TempBranchData, MessageQueue, Name, AccList)
                end,
                [],
                TempProcPidMap
            ),
    case NewBranchList =:= [] of
        true ->
            case OpDone of
                true -> [TempBranchData];
                false -> []
            end;
        false ->
            NewBranchList
    end.

%%% Generate new branches for each message accepted from an actor
gen_branch_foreach_mess(BranchData, MessageQueue, ProcName, BaseList) ->
    lists:foldl(
        fun(Message, AccList) ->
            %%% Check if there's an edge who accepts the message
            DupData = dup_branch(BranchData),
            NewMap = DupData#branch.proc_pid_m,
            NewPid = maps:get(ProcName, NewMap),
            case manage_recv(NewPid, Message) of
                ?UNDEFINED ->
                    stop_processes(NewMap),
                    AccList;
                %%% If an edge has been found, duplicate the branch and add the transition to the graph
                EdgeFound ->
                    % io:fwrite("[RECV] Mess ~p Edge choose ~p~n", [Message, EdgeFound]),
                    ProcFrom = Message#message.from,
                    MessData = Message#message.data,
                    PidFrom = maps:get(ProcFrom, NewMap),
                    Label = format_send_label(ProcFrom, ProcName, MessData),
                    % io:fwrite("~n~n[RECV] LABEL ~ts~n~n", [Label]),
                    EFromInfo = actor_emul:get_proc_edge_info(PidFrom, Message#message.edge),
                    EToInfo = actor_emul:get_proc_edge_info(NewPid, EdgeFound),
                    {LastVertex, NewStateMap} = complex_add_vertex(
                        ProcFrom, EFromInfo, ProcName, EToInfo, DupData, Label
                    ),
                    actor_emul:del_proc_mess_queue(NewPid, Message),
                    %%% NOTE: the last operation MUST be the use_proc_transition, otherwise the final graph might be wrong
                    actor_emul:use_proc_transition(NewPid, EdgeFound),
                    AccList ++ [DupData#branch{last_vertex = LastVertex, states_m = NewStateMap}]
            end
        end,
        BaseList,
        MessageQueue
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
            actor_emul:set_proc_data(NewPid, actor_emul:get_proc_data(V)),
            maps:put(K, NewPid, A)
        end,
        #{},
        ProcMap
    ).

%%% Remove the number from an actor's identificator
remove_id_from_proc(ProcId) ->
    Split = string:split(share:atol(ProcId), ?SEPARATOR),
    case Split of
        [Name | N] ->
            {share:ltoa(Name), N};
        _ ->
            io:fwrite("Error: should have an id ~p", [ProcId]),
            {ProcId, 0}
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
    EL = actor_emul:get_proc_edges(ProcPid),
    ELLength = length(EL),
    if
        ELLength =:= 0 ->
            {Data, false, []};
        ELLength =:= 1 ->
            E = share:first(EL),
            EI = actor_emul:get_proc_edge_info(ProcPid, E),
            {D, B} = eval_edge(EI, ProcName, ProcPid, Data),
            {D, B, []};
        true ->
            Cond = is_lists_edgerecv(ProcPid, EL),
            case Cond of
                true ->
                    {Data, false, []};
                false ->
                    LL = lists:foldl(
                        fun(ItemE, L) ->
                            DD = dup_branch(Data),
                            PP = maps:get(ProcName, DD#branch.proc_pid_m),
                            EI = actor_emul:get_proc_edge_info(PP, ItemE),
                            {D, B} = eval_edge(EI, ProcName, PP, DD),
                            case B of
                                true -> L ++ [D];
                                false -> L
                            end
                        end,
                        [],
                        EL
                    ),
                    case LL =:= [] of
                        true ->
                            {Data, false, []};
                        false ->
                            [F | H] = LL,
                            {F, true, H}
                    end
            end
    end.

%%% Given a list of edges, check if one is a receive edge
is_lists_edgerecv(ProcPid, EL) ->
    lists:foldl(
        fun(E, A) ->
            {E, _, _, Label} = actor_emul:get_proc_edge_info(ProcPid, E),
            A or is_substring(share:atol(Label), "receive")
        end,
        false,
        EL
    ).

%%% Evaluate a transition from an actor
eval_edge(EdgeInfo, ProcName, ProcPid, BData) ->
    {Edge, _, _, PLabel} = EdgeInfo,
    % io:fwrite("Proc ~p eval label ~p~n", [ProcName, PLabel]),
    SLabel = share:atol(PLabel),
    IsArg = is_substring(SLabel, "arg"),
    IsSpawn = is_substring(SLabel, "spawn"),
    IsSend = is_substring(SLabel, "send"),
    if
        IsArg ->
            actor_emul:use_proc_transition(ProcPid, Edge),
            {BData, true};
        IsSpawn ->
            {VNew, NewM} = add_spawn_to_global(SLabel, ProcName, BData),
            NewBData = BData#branch{last_vertex = VNew, proc_pid_m = NewM},
            actor_emul:use_proc_transition(ProcPid, Edge),
            {NewBData, true};
        IsSend ->
            manage_send(SLabel, BData, ProcName, ProcPid, Edge);
        true ->
            {BData, false}
    end.

%%% If SubS is substring of S return True, otherwise False.
is_substring(S, SubS) ->
    is_list(string:find(S, SubS)).

%%% Add a spawn transition to the global view
add_spawn_to_global(SLabel, EmulProcName, Data) ->
    % get proc name
    FunSpawned = string:prefix(SLabel, "spawn "),
    {FunSName, Counter} = remove_id_from_proc(FunSpawned),
    % spawn the actor emulator
    FuncPid = spawn(actor_emul, proc_loop, [#actor_info{fun_name = FunSName, id = Counter}]),
    NewMap = maps:put(share:ltoa(FunSpawned), FuncPid, Data#branch.proc_pid_m),
    % get spawn arguemnt and add them to the local variables of the actor
    LocalList = get_local_vars(EmulProcName, SLabel, FunSName),
    lists:foreach(fun(Var) -> actor_emul:add_proc_spawnvars(FuncPid, Var) end, LocalList),
    % create the edge on the global graph
    VNew = share:add_vertex(Data#branch.graph),
    NewLabel = share:atol(EmulProcName) ++ "Δ" ++ FunSpawned,
    digraph:add_edge(Data#branch.graph, Data#branch.last_vertex, VNew, NewLabel),
    {VNew, NewMap}.

get_local_vars(ProcId, Label, FunSName) ->
    EM = share:get_edgedata(element(1, remove_id_from_proc(ProcId))),
    InputData = maps:get(Label, EM, []),
    % io:fwrite("[GV] EmulProcName ~p Label ~p Input ~p~n", [FunSName, Label, EM]),
    % add input data to local vars
    case InputData of
        [] ->
            [];
        _ ->
            [{_, Input}] = ets:lookup(?ARGUMENTS, share:atol(FunSName)),
            % io:fwrite("[GV] for fun ~p found ~p~n", [atol(FunSName), Input]),
            {LL, Remain} = lists:foldl(
                fun({var, _, Name}, {A, In}) ->
                    case In of
                        [] ->
                            io:fwrite("ERROR: list should NOT be empty but there is ~p~n", Name),
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
                false -> io:fwrite("ERROR: list should be empty but there is ~p~n", Remain)
            end,
            LL
    end.

%%% Evaluate a send transition of an actor
manage_send(SendLabel, Data, ProcName, ProcPid, Edge) ->
    ProcPidMap = Data#branch.proc_pid_m,
    DataSent = get_data_from_label(SendLabel),
    ProcSentTemp = share:ltoa(get_proc_from_label(SendLabel)),
    IsVar = share:is_erlvar(ProcSentTemp),
    ProcSentName =
        case IsVar of
            true -> check_vars(ProcPid, ProcSentTemp);
            false -> share:ltoa(check_pid_self(ProcSentTemp, ProcName))
        end,
    ProcSentPid = maps:get(ProcSentName, ProcPidMap, no_pid),
    case ProcSentPid of
        no_pid ->
            io:fwrite("[SEND-ERR] no pid found for: ~p~n", [ProcSentTemp]),
            {Data, false};
        P ->
            actor_emul:add_proc_mess_queue(P, new_message(ProcName, DataSent, Edge)),
            %%% NOTE: the last operation MUST be use_proc_transition, otherwise the final graph might be wrong
            actor_emul:use_proc_transition(ProcPid, Edge),
            {Data, true}
    end.

%%% Evaluate a receive transition of an actor
manage_recv(ProcPid, Message) ->
    EdgeList = actor_emul:get_proc_edges(ProcPid),
    IsRecvList = is_lists_edgerecv(ProcPid, EdgeList),
    %io:fwrite("IsRECV ~p EL ~p~n", [IsRecv, EL]),
    From = Message#message.from,
    case IsRecvList of
        false ->
            % TODO: manage when is not ONLY a receive edge, like:
            %         3
            %        /recv
            % 1 -> 2
            %        \send
            %         4
            %
            % The following piece of code is old, check before uncommenting
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
            %%% IMPORTANT TODO: evaluation of the edge's order not implemented!!
            %%% Evaluate edges in order as in the code
            {_, EdgeChoosen} = lists:foldl(
                fun(Edge, {AlreadyFound, RetEdge}) ->
                    case AlreadyFound of
                        true ->
                            {AlreadyFound, RetEdge};
                        false ->
                            {_, _, _, ELabel} = actor_emul:get_proc_edge_info(ProcPid, Edge),
                            IsCompatible = is_pm_msg_compatible(ProcPid, From, ELabel, Message),
                            case IsCompatible of
                                true -> {true, Edge};
                                false -> {AlreadyFound, RetEdge}
                            end
                    end
                end,
                {false, ?UNDEFINED},
                EdgeList
            ),
            EdgeChoosen
    end.

%%% Find the actor id from a variable's list, given the variable name
check_vars(ProcPid, VarName) ->
    ProcLocalVars = actor_emul:get_proc_localvars(ProcPid),
    % io:fwrite("Find var ~p in ~p pid ~p~n", [VarName, ProcLocalVars, ProcPid]),
    VarValue = share:find_var(ProcLocalVars, VarName),
    case VarValue of
        not_found ->
            VarName;
        V ->
            case V#variable.type of
                ?UNDEFINED -> VarName;
                "pid" -> V#variable.value;
                pid -> V#variable.value;
                _ -> V#variable.type
            end
    end.

%%% Remove the "pid_" part from a variable's type
% remove_pid_part(Data) -> ltoa(lists:flatten(string:replace(atol(Data), "pid_", ""))).

%%% Check if a pattern metching match a message, then register the new variables
is_pm_msg_compatible(ProcPid, CallingProc, PatternMatching, Message) ->
    {IsCompatible, ToRegisterList} = check_msg_comp(
        ProcPid, CallingProc, PatternMatching, Message#message.data
    ),
    % io:fwrite("Reg List ~p~n", [RegList]),
    lists:foreach(
        fun(Item) -> register_var(Item) end,
        ToRegisterList
    ),
    IsCompatible.

%%% Check if a pattern metching match a message
check_msg_comp(ProcPid, CallingProc, PatternMatching, Message) ->
    MessageS = share:atol(Message),
    PatternMS = lists:flatten(string:replace(share:atol(PatternMatching), "receive ", "")),
    [FirstPtmtChar | RestPtmt] = PatternMS,
    [FirstMessChar | RestMess] = MessageS,
    IsFirstCharUpperCase = share:is_erlvar(PatternMS),
    if
        ([FirstPtmtChar] =:= "{") and ([FirstMessChar] =:= "{") ->
            check_tuple(ProcPid, CallingProc, RestPtmt, RestMess);
        PatternMS =:= MessageS ->
            {true, []};
        IsFirstCharUpperCase ->
            {true, [{ProcPid, share:ltoa(PatternMS), check_pid_self(Message, CallingProc)}]};
        [FirstPtmtChar] =:= "_" ->
            {true, []};
        true ->
            {false, []}
    end.

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
    and_rec(BoolList).

%%% Register a actor's variable
register_var(Data) ->
    {ProcPid, Name, Value} = Data,
    %%% type = pid to change, for now it's ok like this because I only focus on pid exchange
    V = #variable{name = share:ltoa(Name), type = pid, value = share:ltoa(Value)},
    % io:fwrite("Added Var ~p~n", [V]),
    actor_emul:add_proc_localvars(ProcPid, V).

%%% Substitute pif_self to pid_ProcId
check_pid_self(Data, ProcId) ->
    % io:fwrite("[C]Data ~p proc id ~p~n", [Data, ProcId]),
    lists:flatten(string:replace(share:atol(Data), "pid_self", share:atol(ProcId))).

%%% Custom recursive and operation
and_rec([]) ->
    {true, []};
and_rec([{B, L} | T]) ->
    case B of
        true ->
            {A, LL} = and_rec(T),
            {A, L ++ LL};
        false ->
            {B, []}
    end.

%%% Add a vertex to the global view, with some checks for recusive edges
complex_add_vertex(Proc1, EdgeInfo1, Proc2, EdgeInfo2, Data, Label) ->
    StateM = Data#branch.states_m,
    VLast = Data#branch.last_vertex,
    G = Data#branch.graph,
    {_, V1, V2, _} = EdgeInfo1,
    {_, PV1, PV2, _} = EdgeInfo2,
    EL = digraph:out_edges(G, VLast),
    %%% Check if these vertex correspond to a global view's vertex
    Vfirst = maps:get({{Proc1, V2}, {Proc2, PV2}}, StateM, ?UNDEFINED),
    %%% Recheck vertexs, but in another order
    Vsecond = maps:get({{Proc2, PV2}, {Proc1, V2}}, StateM, ?UNDEFINED),
    %%% Check if already exist the same transition
    case check_same_label(G, EL, Label) of
        nomatch ->
            case Vfirst of
                ?UNDEFINED ->
                    case Vsecond of
                        ?UNDEFINED ->
                            Cond = (V1 =:= V2) and (PV1 =:= PV2),
                            case Cond of
                                true ->
                                    digraph:add_edge(G, VLast, VLast, Label),
                                    NewM = maps:put({{Proc1, V1}, {Proc2, PV1}}, VLast, StateM),
                                    {VLast, NewM};
                                %%% Add a new vertex, because no match found in StateM
                                false ->
                                    VAdded = share:add_vertex(G),
                                    digraph:add_edge(G, VLast, VAdded, Label),
                                    NewM = maps:put({{Proc1, V1}, {Proc2, PV1}}, VLast, StateM),
                                    {VAdded, NewM}
                            end;
                        _ ->
                            %%% Match second vertex
                            digraph:add_edge(G, VLast, Vsecond, Label),
                            NewM = maps:put({{Proc1, V1}, {Proc2, PV1}}, VLast, StateM),
                            {Vsecond, NewM}
                    end;
                _ ->
                    %%% Match First vertex
                    digraph:add_edge(G, VLast, Vfirst, Label),
                    {Vfirst, StateM}
            end;
        VRet ->
            {VRet, StateM}
    end.

%%% Returns the outgoing vertex given a label and a list of transition
check_same_label(G, EL, Label) ->
    lists:foldl(
        fun(E, A) ->
            {E, _, VTo, VLabel} = digraph:edge(G, E),
            case VLabel =:= Label of
                true -> VTo;
                false -> A
            end
        end,
        nomatch,
        EL
    ).

%%% Get the data from a send local view's label
get_data_from_label(S) ->
    Ret = lists:nth(2, string:split(S, " ", all)),
    FirstChar = share:first(Ret),
    if
        [FirstChar] =:= "[" -> Ret ++ " " ++ lists:nth(3, string:split(S, " ", all));
        true -> Ret
    end.

%%% Get the process from a send local view's label
get_proc_from_label(S) ->
    lists:reverse(lists:nth(1, string:split(lists:reverse(S), " ", all))).

%%% Stop all the processes from the process map
stop_processes(DataL) when is_list(DataL) ->
    lists:foreach(
        fun(D) ->
            M = D#branch.proc_pid_m,
            stop_processes(M)
        end,
        DataL
    );
stop_processes(ProcMap) ->
    maps:foreach(fun(_K, V) -> V ! stop end, ProcMap).
