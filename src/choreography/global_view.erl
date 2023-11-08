-module(global_view).
-include("../share/common_data.hrl").

%%% API
-export([generate/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%% Generate the glabal view from an entrypoint and save it in a specified folder
generate(OutputDir, EntryPoint) ->
    MainGraph = db_manager:get_fun_graph(EntryPoint),
    case MainGraph of
        no_graph_found ->
            no_entry_point_found;
        _ ->
            G = create_globalview(EntryPoint),
            {ok, [In]} = io:fread("Minimize global view? [y/n] ", "~a"),
            CG =
                case In of
                    n -> G;
                    _ -> fsa:minimize(G)
                end,
            common_fun:save_graph_to_file(CG, OutputDir, atol(EntryPoint), global),
            finished
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%% Create the glabal view from a function entrypoint name
create_globalview(Name) ->
    RetG = digraph:new(),
    VNew = common_fun:add_vertex(RetG),
    MainProcPid = spawn(actor_emul, proc_loop, [#proc_info{proc_id = Name}]),
    ProcPidMap = #{Name => MainProcPid},
    % initialize first branch
    progress_procs(RetG, [new_branch(RetG, VNew, ProcPidMap)]).

%%% Create a new branch object
new_branch(G, V, P) ->
    #branch{
        graph = G,
        last_vertex = V,
        proc_pid_m = P
    }.

%%% Create a new message object
new_message(F, D, E) ->
    #message{
        from = F, data = D, edge = E
    }.

%%% Explore every possible branch of executions
progress_procs(G, []) ->
    G;
progress_procs(GlobalGraph, BranchList) when is_list(BranchList) ->
    RealList = lists:flatten(BranchList),
    %io:fwrite("Branch to eval ~p~n", [length(RealList)]),
    NewBL = lists:foldl(
        fun(Item, AccL) ->
            %io:fwrite("Eval branch~n"),
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
                    %io:fwrite("[RECV] Mess ~p Edge choose ~p~n", [Message, EdgeFound]),
                    ProcFrom = Message#message.from,
                    MessData = Message#message.data,
                    PidFrom = maps:get(ProcFrom, NewMap),
                    Label = format_send_label(ProcFrom, ProcName, MessData),
                    % %io:fwrite("~n~n[RECV] LABEL ~ts~n~n", [Label]),
                    EFromInfo = actor_emul:get_proc_edge_info(PidFrom, Message#message.edge),
                    EToInfo = actor_emul:get_proc_edge_info(NewPid, EdgeFound),
                    %LastVertex =  simple_add_vertex(DupData, Label),
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
    atol(ProcFrom) ++ "→" ++ atol(ProcTo) ++ ":" ++ atol(Data).

%%% Create a duplicate for a branch object
dup_branch(Data) ->
    Data#branch{proc_pid_m = duplicate_proccess(Data#branch.proc_pid_m)}.

%%% Duplicate ideantical processes from a process' map
duplicate_proccess(ProcMap) ->
    maps:fold(
        fun(K, V, A) ->
            Name = remove_id_from_proc(K),
            NewPid = spawn(actor_emul, proc_loop, [#proc_info{proc_id = Name}]),
            actor_emul:set_proc_data(NewPid, actor_emul:get_proc_data(V)),
            maps:put(K, NewPid, A)
        end,
        #{},
        ProcMap
    ).

%%% Remove the number from an actor's identificator
remove_id_from_proc(ProcId) ->
    SProcId = atol(ProcId),
    {Name, N} = lists:split(length(SProcId) - 1, SProcId),
    case catch list_to_integer(N) of
        {'EXIT', _} -> ProcId;
        _ -> ltoa(remove_last(Name))
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
            E = common_fun:first(EL),
            EI = actor_emul:get_proc_edge_info(ProcPid, E),
            {D, B} = eval_edge(EI, ProcName, ProcPid, Data),
            {D, B, []};
        true ->
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
    end.

%%% Given a list of edges, check if everyone is a receive edge
is_lists_edgerecv(ProcPid, EL) ->
    lists:foldl(
        fun(E, A) ->
            {E, _, _, Label} = actor_emul:get_proc_edge_info(ProcPid, E),
            SLabel = atol(Label),
            IsRecv = string:find(SLabel, "receive"),
            A and is_list(IsRecv)
        end,
        true,
        EL
    ).

%%% Evaluate a transition from an actor
eval_edge(EdgeInfo, ProcName, ProcPid, BData) ->
    {Edge, _, _, PLabel} = EdgeInfo,
    %io:fwrite("Proc ~p eval label ~p~n", [ProcName, PLabel]),
    SLabel = atol(PLabel),
    IsArg = is_list(string:find(SLabel, "arg")),
    IsSpawn = is_list(string:find(SLabel, "spawn")),
    IsSend = is_list(string:find(SLabel, "send")),
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

%%% Add a spanw transition to the global view
add_spawn_to_global(SLabel, ProcName, Data) ->
    ProcId = string:prefix(SLabel, "spawn "),
    FuncName = remove_last(remove_last(ProcId)),
    FuncPid = spawn(actor_emul, proc_loop, [#proc_info{proc_id = ltoa(FuncName)}]),
    NewMap = maps:put(ltoa(ProcId), FuncPid, Data#branch.proc_pid_m),
    VNew = common_fun:add_vertex(Data#branch.graph),
    %%% Δ means spawned
    NewLabel = atol(ProcName) ++ "Δ" ++ ProcId,
    digraph:add_edge(Data#branch.graph, Data#branch.last_vertex, VNew, NewLabel),
    {VNew, NewMap}.

%%% Remove the last element froom a list
remove_last(List) when is_list(List) ->
    {Rest, _} = lists:split(length(List) - 1, List),
    Rest.

%%% Evaluate a send transition of an actor
manage_send(SLabel, Data, ProcName, ProcPid, Edge) ->
    ProcPidMap = Data#branch.proc_pid_m,
    DataSent = get_data_from_label(SLabel),
    ProcSentTemp = ltoa(get_proc_from_label(SLabel)),
    IsVar = common_fun:is_erlvar(ProcSentTemp),
    ProcSentName =
        case IsVar of
            true -> check_vars(ProcName, ProcPid, ProcSentTemp);
            false -> ProcSentTemp
        end,
    ProcSentPid = maps:get(ProcSentName, ProcPidMap, no_pid),
    case ProcSentPid of
        no_pid ->
            %io:fwrite("[SEND-ERR] no pid found for: ~p~n", [ProcSentName]),
            {Data, false};
        P ->
            actor_emul:add_proc_mess_queue(P, new_message(ProcName, DataSent, Edge)),
            %%% NOTE: the last operation MUST be the use_proc_transition, otherwise the final graph might be wrong
            actor_emul:use_proc_transition(ProcPid, Edge),
            {Data, true}
    end.

%%% Evaluate a receive transition of an actor
manage_recv(ProcPid, Message) ->
    EL = actor_emul:get_proc_edges(ProcPid),
    %%% TODO: trovare il modo di valutare in ordine i rami del receive (in quanto è molto rilevante nell'esecuzione)
    IsRecv = is_lists_edgerecv(ProcPid, EL),
    From = Message#message.from,
    case IsRecv of
        false ->
            % TODO: gestire casistica, il seguente codice è vecchio e quindi da prendere con le pinze
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
            {_, EdgeChoosen} = lists:foldl(
                fun(E, {B, Ret}) ->
                    case B of
                        true ->
                            {B, Ret};
                        false ->
                            {E, _, _, ELabel} = actor_emul:get_proc_edge_info(ProcPid, E),
                            IsIt = is_pm_msg_compatible(ProcPid, From, ELabel, Message),
                            case IsIt of
                                true -> {true, E};
                                false -> {B, Ret}
                            end
                    end
                end,
                {false, ?UNDEFINED},
                EL
            ),
            EdgeChoosen
    end.

%%% Find the actor id from a variable's list, given the variable name
check_vars(ProcName, ProcPid, VarName) ->
    Name = remove_id_from_proc(ProcName),
    GlobalViewLocalVars = actor_emul:get_proc_localvars(ProcPid),
    LocalViewLocalVars = db_manager:get_fun_local_vars(Name),
    SpInfoP = find_spawn_info(ProcName),
    ArgsVars =
        case SpInfoP =:= [] of
            true ->
                [];
            false ->
                convertL_in_variable(
                    SpInfoP#spawned_proc.args_called, SpInfoP#spawned_proc.args_local
                )
        end,
    SeachList = ArgsVars ++ LocalViewLocalVars ++ GlobalViewLocalVars,
    %io:fwrite("Find var ~p in ~p from ~p~n", [VarName, SeachList, ProcName]),
    VarValue = find_var(SeachList, VarName),
    case VarValue of
        nomatch ->
            VarName;
        V ->
            case V#variable.type of
                ?UNDEFINED -> VarName;
                "self" -> SpInfoP#spawned_proc.called_where;
                self -> SpInfoP#spawned_proc.called_where;
                "pid_self" -> SpInfoP#spawned_proc.called_where;
                pid_self -> SpInfoP#spawned_proc.called_where;
                _ -> remove_pid_part(V#variable.type)
            end
    end.

%%% Remove the "pid_" part from a variable's type
remove_pid_part(Data) ->
    ltoa(lists:flatten(string:replace(atol(Data), "pid_", ""))).

%%% Get the spawn info given an actor id
find_spawn_info(PId) ->
    SpInfoAll = db_manager:get_spawn_info(),
    common_fun:first(lists:filter(fun(S) -> S#spawned_proc.name =:= PId end, SpInfoAll)).

%%% Convert a list AST in a variable's list
convertL_in_variable(A, B) ->
    convertL_in_variable(A, B, []).
convertL_in_variable({nil, _}, [], L) ->
    L;
convertL_in_variable({cons, _, HeadList, TailList}, [H | T], L) ->
    {var, _, Name} = H,
    Var = convert_in_variable(HeadList, Name),
    AL = convertL_in_variable(TailList, T, L),
    [Var] ++ AL;
convertL_in_variable(_, _, _) ->
    [].

%%% Evaluate an AST entry and converts it to a variable
convert_in_variable(Eval, Name) ->
    TempV = #variable{name = Name},
    case Eval of
        {cons, _, HeadList, TailList} ->
            Var = convert_in_variable(HeadList, ?UNDEFINED),
            VarT = convert_in_variable(TailList, ?UNDEFINED),
            NewVal = [Var] ++ VarT#variable.value,
            TempV#variable{value = NewVal};
        %%% Evaluate Types
        {call, _, {atom, _, self}, _} ->
            TempV#variable{type = ltoa("pid_self")};
        {integer, _, Val} ->
            TempV#variable{type = integer, value = Val};
        {float, _, Val} ->
            TempV#variable{type = float, value = Val};
        {string, _, Val} ->
            TempV#variable{type = string, value = Val};
        {atom, _, Val} ->
            TempV#variable{type = atom, value = Val};
        {tuple, _, TupleVal} ->
            L = lists:foldl(
                fun(I, A) ->
                    V = convert_in_variable(I, ?UNDEFINED),
                    A ++ [V]
                end,
                [],
                TupleVal
            ),
            TempV#variable{type = tuple, value = L};
        {nil, _} ->
            TempV#variable{type = list, value = []};
        _ ->
            TempV
    end.

%%% Find a variable in a list, given the name
find_var([], _) ->
    nomatch;
find_var([H | T], VarName) ->
    Cond = H#variable.name =:= VarName,
    case Cond of
        true -> H;
        false -> find_var(T, VarName)
    end.

%%% Check if a pattern metching match a message, then register the new variables
is_pm_msg_compatible(ProcPid, CallingProc, PatternMatching, Message) ->
    {RetBool, RegList} = check_msg_comp(
        ProcPid, CallingProc, PatternMatching, Message#message.data
    ),
    lists:foreach(
        fun(Item) -> register_var(Item) end,
        RegList
    ),
    RetBool.

%%% Check if a pattern metching match a message
check_msg_comp(ProcPid, CallingProc, PatternMatching, Message) ->
    MessageS = atol(Message),
    PatternMS = lists:flatten(string:replace(atol(PatternMatching), "receive ", "")),
    [FirstPChar | RestP] = PatternMS,
    [FirstMChar | RestM] = MessageS,
    IsFirstCharUpperCase = common_fun:is_erlvar(PatternMS),
    if
        %%% hierarchy
        ([FirstPChar] =:= "{") and ([FirstMChar] =:= "{") ->
            ContentP = remove_last(RestP),
            ContentM = remove_last(RestM),
            PL = string:split(ContentP, ",", all),
            A = lists:enumerate(PL),
            ML = string:split(ContentM, ",", all),
            B = lists:enumerate(ML),
            BoolList = [
                check_msg_comp(ProcPid, CallingProc, IA, IB)
             || {BA, IA} <- A, {BB, IB} <- B, BA =:= BB
            ],
            and_rec(BoolList);
        PatternMS =:= MessageS ->
            {true, []};
        IsFirstCharUpperCase ->
            {true, [{ProcPid, ltoa(PatternMS), check_pid_self(Message, CallingProc)}]};
        [FirstPChar] =:= "_" ->
            {true, []};
        true ->
            {false, []}
    end.

%%% Register a actor's variable
register_var(Data) ->
    {ProcPid, Name, Type} = Data,
    V = #variable{name = ltoa(Name), type = ltoa(Type)},
    %io:fwrite("Added Var ~p~n", [V]),
    actor_emul:add_proc_localvars(ProcPid, V).

%%% Substitute pif_self to pid_procId
check_pid_self(Data, ProcId) ->
    % %io:fwrite("[C]Data ~p proc id ~p~n", [Data, ProcId]),
    lists:flatten(string:replace(atol(Data), "pid_self", "pid_" ++ atol(ProcId))).

%%% Custom recursive logic and
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

%%% Add a send/recv vertex to the global view, with some checks for recusive edges
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
                                    VAdded = common_fun:add_vertex(G),
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
                    %io:fwrite("[ADD]First defined!!~n"),
                    digraph:add_edge(G, VLast, Vfirst, Label),
                    {Vfirst, StateM}
            end;
        VRet ->
            {VRet, StateM}
    end.

% simple_add_vertex(Data, Label) ->
%     VLast = Data#branch.last_vertex,
%     G = Data#branch.graph,
%     VAdded = common_fun:add_vertex(G),
%     digraph:add_edge(G, VLast, VAdded, Label),
%     VAdded.

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
    FirstChar = common_fun:first(Ret),
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

atol(A) when is_list(A) -> A;
atol(A) when is_atom(A) -> atom_to_list(A).
ltoa(L) when is_atom(L) -> L;
ltoa(L) when is_list(L) -> list_to_atom(L).
