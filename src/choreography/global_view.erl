-module(global_view).
-include("../share/common_data.hrl").

%%% API
-export([generate/2, proc_loop/1]).

%%%===================================================================
%%% API
%%%===================================================================

generate(OutputDir, EntryPoint) ->
    MainGraph = db_manager:get_fun_graph(EntryPoint),
    case MainGraph of
        no_graph_found -> no_entry_point_found;
        _ -> generate_global(EntryPoint, OutputDir)
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

generate_global(EntryPoint, Dir) ->
    Gr = create_globalview(EntryPoint),
    MG = fsa:minimize(Gr),
    common_fun:save_graph_to_file(MG, Dir, atol(EntryPoint), global),
    finished.

create_globalview(Name) ->
    RetG = digraph:new(),
    VNew = common_fun:add_vertex(RetG),
    MainProcPid = spawn(?MODULE, proc_loop, [#proc_info{proc_id = Name}]),
    ProcPidMap = #{Name => MainProcPid},
    %%% initialize first branch
    progress_proc(RetG, [new_branch(RetG, VNew, ProcPidMap, #{})]).

new_branch(G, V, P, M) ->
    #branch{
        graph = G,
        last_vertex = V,
        proc_pid_m = P,
        states_m = M
    }.

new_message(F, D, E) ->
    #message{
        from = F, data = D, edge = E
    }.

progress_proc(G, []) ->
    G;
progress_proc(GlobalGraph, BranchList) when is_list(BranchList) ->
    io:fwrite("Branch to eval ~p~n", [length(BranchList)]),
    NewBL = lists:foldl(
        fun(Item, AccL) ->
            io:fwrite("Eval branch~n"),
            NewBreanches = progress_single_branch(Item),
            AccL ++ NewBreanches
        end,
        [],
        BranchList
    ),
    progress_proc(GlobalGraph, NewBL).

progress_single_branch(BData) ->
    {TempBranchData, TempBranchList, OpDone} = maps:fold(
        fun(Name, Pid, AccData) -> eval_proc_until_recv(Name, Pid, AccData) end,
        {BData, [], false},
        BData#branch.proc_pid_m
    ),
    TempProcPidMap = TempBranchData#branch.proc_pid_m,
    NewBranchList = maps:fold(
        fun(Name, Pid, AccList) ->
            MessageQueue = get_proc_mess_queue(Pid),
            gen_branch_foreach_mess(TempBranchData, MessageQueue, Name, Pid, AccList)
        end,
        TempBranchList,
        TempProcPidMap
    ),
    case NewBranchList =:= [] of
        true ->
            case OpDone of
                true -> progress_single_branch(TempBranchData);
                false -> []
            end;
        false ->
            stop_processes(TempProcPidMap),
            NewBranchList
    end.

gen_branch_foreach_mess(BranchData, MessageQueue, ProcName, ProcPid, BaseList) ->
    lists:foldl(
        fun(Message, ALL) ->
            ProcFrom = Message#message.from,
            MessData = Message#message.data,
            {Done, ETo} = manage_recv(ProcPid, Message),
            case Done of
                true ->
                    DupData = dup_branch(BranchData),
                    NewPid = maps:get(ProcName, DupData#branch.proc_pid_m),
                    PidFrom = maps:get(ProcFrom, DupData#branch.proc_pid_m),
                    Label = format_send_label(ProcFrom, ProcName, MessData),
                    EFromInfo = get_proc_edge_info(PidFrom, Message#message.edge),
                    EToInfo = get_proc_edge_info(NewPid, ETo),
                    {LastVertex, NewStateMap} = add_vertex_to_graph(
                        ProcFrom, EFromInfo, ProcName, EToInfo, DupData, Label
                    ),
                    del_proc_mess_queue(NewPid, Message),
                    NewPid ! {use_transition, ETo},
                    ALL ++ [DupData#branch{last_vertex = LastVertex, states_m = NewStateMap}];
                false ->
                    ALL
            end
        end,
        BaseList,
        MessageQueue
    ).

format_send_label(ProcFrom, ProcTo, Data) ->
    atol(ProcFrom) ++ "→" ++ atol(ProcTo) ++ ":" ++ atol(Data).

dup_branch(Data) ->
    Data#branch{proc_pid_m = duplicate_proccess(Data#branch.proc_pid_m)}.

duplicate_proccess(ProcMap) ->
    maps:fold(
        fun(K, V, A) ->
            {Name, _} = remove_id_with_check(K),
            NewPid = spawn(?MODULE, proc_loop, [#proc_info{proc_id = Name}]),
            set_proc_data(NewPid, get_proc_data(V)),
            maps:put(K, NewPid, A)
        end,
        #{},
        ProcMap
    ).

remove_id_with_check(ProcId) ->
    SProcId = atol(ProcId),
    {Name, N} = lists:split(length(SProcId) - 1, SProcId),
    case catch list_to_integer(N) of
        {'EXIT', _} -> {ProcId, N};
        _ -> {ltoa(remove_last(Name)), N}
    end.

eval_proc_until_recv(ProcName, ProcPid, AccData) ->
    ProcOD = get_proc_out_degree(ProcPid),
    {Data, NBL, Bool} = AccData,
    {NewData, NL, OpDone} =
        if
            %%% TODO: verify this -> No out edges equals to final state?
            ProcOD =:= 0 ->
                {Data, [], false};
            ProcOD =:= 1 ->
                EToEval = choose_edge(ProcPid),
                case EToEval of
                    false ->
                        {Data, [], false};
                    E ->
                        {D, O} = eval_edge(E, ProcName, ProcPid, Data),
                        {D, [], O}
                end;
            true ->
                {Data, [], false}
        end,
    case OpDone of
        false -> {NewData, NBL, Bool};
        true -> eval_proc_until_recv(ProcName, ProcPid, {NewData, NBL ++ NL, true})
    end.

is_lists_edgerecv(ProcPid, EL) ->
    lists:foldl(
        fun(E, A) ->
            {E, _, _, Label} = get_proc_edge_info(ProcPid, E),
            SLabel = atol(Label),
            IsRecv = string:find(SLabel, "receive"),
            A and is_list(IsRecv)
        end,
        true,
        EL
    ).

choose_edge(ProcPid) ->
    EL = get_proc_edges(ProcPid),
    E = common_fun:first(EL),
    get_proc_edge_info(ProcPid, E).

eval_edge(EdgeInfo, ProcName, ProcPid, BData) ->
    {Edge, _, _, PLabel} = EdgeInfo,
    io:fwrite("Proc ~p eval label ~p~n", [ProcName, PLabel]),
    SLabel = atol(PLabel),
    IsArg = string:find(SLabel, "arg"),
    IsSpawn = string:find(SLabel, "spawn"),
    IsSend = string:find(SLabel, "send"),
    if
        is_list(IsArg) ->
            ProcPid ! {use_transition, Edge},
            {BData, true};
        is_list(IsSpawn) ->
            ProcPid ! {use_transition, Edge},
            {VNew, NewM} = add_spawn_to_global(SLabel, ProcName, BData),
            NewBData = BData#branch{last_vertex = VNew, proc_pid_m = NewM},
            {NewBData, true};
        is_list(IsSend) ->
            manage_send(SLabel, BData, ProcName, Edge);
        true ->
            {BData, false}
    end.

add_spawn_to_global(SLabel, ProcName, Data) ->
    ProcId = string:prefix(SLabel, "spawn "),
    FuncName = remove_last(remove_last(ProcId)),
    FuncPid = spawn(?MODULE, proc_loop, [#proc_info{proc_id = ltoa(FuncName)}]),
    NewMap = maps:put(ltoa(ProcId), FuncPid, Data#branch.proc_pid_m),
    VNew = common_fun:add_vertex(Data#branch.graph),
    %%% Δ means spawned
    NewLabel = atol(ProcName) ++ "Δ" ++ ProcId,
    digraph:add_edge(Data#branch.graph, Data#branch.last_vertex, VNew, NewLabel),
    {VNew, NewMap}.

remove_last(A) ->
    {H, _} = lists:split(length(A) - 1, A),
    H.

manage_send(SLabel, Data, ProcName, Edge) ->
    ProcPidMap = Data#branch.proc_pid_m,
    ProcPid = maps:get(ProcName, ProcPidMap),
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
        no_pid -> io:fwrite("[SEND] no pid found for: ~p~n", [ProcSentName]);
        P -> add_proc_mess_queue(P, new_message(ProcName, DataSent, Edge))
    end,
    ProcPid ! {use_transition, Edge},
    {Data, true}.

manage_recv(ProcPid, Message) ->
    EL = get_proc_edges(ProcPid),
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
            {false, {}};
        true ->
            {_, EdgeChoosen} = lists:foldl(
                fun(E, {B, Ret}) ->
                    case B of
                        true ->
                            {B, Ret};
                        false ->
                            {E, _, _, ELabel} = get_proc_edge_info(ProcPid, E),
                            IsIt = is_message_compatible(ProcPid, From, ELabel, Message),
                            case IsIt of
                                true -> {true, E};
                                false -> {B, Ret}
                            end
                    end
                end,
                {false, ?UNDEFINED},
                EL
            ),
            io:fwrite("[RECV] Mess ~p Edge choose ~p~n", [Message, EdgeChoosen]),
            case EdgeChoosen of
                ?UNDEFINED -> {false, {}};
                _ -> {true, EdgeChoosen}
            end
    end.

check_vars(ProcName, ProcPid, VarName) ->
    SpInfoP = find_spawn_info(ProcName),
    LVars = get_proc_localvars(ProcPid),
    {Name, _} = remove_id_with_check(ProcName),
    LocalVars = db_manager:get_fun_local_vars(Name),
    L =
        if
            SpInfoP =:= [] ->
                [];
            true ->
                convertL_in_variable(
                    SpInfoP#spawned_proc.args_called, SpInfoP#spawned_proc.args_local
                )
        end,
    SeachList = L ++ LocalVars ++ LVars,
    io:fwrite("Find var ~p in ~p from ~p~n", [VarName, SeachList, ProcName]),
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

remove_pid_part(Data) ->
    ltoa(lists:flatten(string:replace(atol(Data), "pid_", ""))).

find_spawn_info(PId) ->
    SpInfoAll = db_manager:get_spawn_info(),
    common_fun:first(lists:filter(fun(S) -> S#spawned_proc.name =:= PId end, SpInfoAll)).

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

find_var([], _) ->
    nomatch;
find_var([H | T], VarName) ->
    Cond = H#variable.name =:= VarName,
    case Cond of
        true -> H;
        false -> find_var(T, VarName)
    end.

is_message_compatible(ProcPid, CallingProc, PatternMatching, Message) ->
    {B, R} = check_mess_comp(ProcPid, CallingProc, PatternMatching, Message#message.data),
    case B of
        true ->
            lists:foreach(
                fun(I) ->
                    {PPid, CallP, PM, M} = I,
                    register_var(PPid, CallP, PM, M)
                end,
                R
            );
        false ->
            ?UNDEFINED
    end,
    B.

check_mess_comp(ProcPid, CallingProc, PatternMatching, Message) ->
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
                check_mess_comp(ProcPid, CallingProc, IA, IB)
             || {BA, IA} <- A, {BB, IB} <- B, BA =:= BB
            ],
            and_rec(BoolList);
        PatternMS =:= MessageS ->
            {true, []};
        IsFirstCharUpperCase ->
            {true, [{ProcPid, CallingProc, ltoa(PatternMS), check_pid_self(Message, CallingProc)}]};
        [FirstPChar] =:= "_" ->
            {true, []};
        true ->
            {false, []}
    end.

register_var(ProcPid, ProcName, Name, Type) ->
    io:fwrite("[Register] PName ~p VName ~p VType ~p~n", [ProcName, Name, Type]),
    IsPid = string:prefix(Type, "pid_"),
    [FC | _] = Type,
    IsLower = common_fun:is_lowercase([FC]),
    TVal =
        if
            is_list(IsPid) -> ltoa("pid_" ++ atol(ProcName));
            IsLower -> atom;
            true -> ltoa(Type)
        end,
    V = #variable{name = ltoa(Name), type = TVal},
    io:fwrite("Added Var ~p~n", [V]),
    add_proc_localvars(ProcPid, V).

check_pid_self(Data, ProcId) ->
    io:fwrite("[C]Data ~p proc id ~p~n", [Data, ProcId]),
    lists:flatten(string:replace(atol(Data), "pid_self", "pid_" ++ atol(ProcId))).

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

add_vertex_to_graph(Proc1, Edge1, Proc2, Edge2, Data, Label) ->
    StateM = Data#branch.states_m,
    VLast = Data#branch.last_vertex,
    G = Data#branch.graph,
    {_, V1, V2, _} = Edge1,
    {_, PV1, PV2, _} = Edge2,
    Vfirst = maps:get({{Proc1, V2}, {Proc2, PV2}}, StateM, ?UNDEFINED),
    case Vfirst of
        ?UNDEFINED ->
            Vsecond = maps:get({{Proc2, PV2}, {Proc1, V2}}, StateM, ?UNDEFINED),
            case Vsecond of
                ?UNDEFINED ->
                    EL = digraph:out_edges(G, VLast),
                    {AlreadyExist, VCase} = lists:foldl(
                        fun(E, A) ->
                            {E, VLast, VTo, VLabel} = digraph:edge(G, E),
                            case VLabel =:= Label of
                                true -> {true, VTo};
                                false -> A
                            end
                        end,
                        {false, VLast},
                        EL
                    ),
                    case AlreadyExist of
                        true ->
                            {VCase, StateM};
                        false ->
                            VAdded = common_fun:add_vertex(G),
                            digraph:add_edge(G, VLast, VAdded, Label),
                            NewM = maps:put({{Proc1, V1}, {Proc2, PV1}}, VLast, StateM),
                            {VAdded, NewM}
                    end;
                _ ->
                    EL = digraph:out_edges(G, VLast),
                    {AlreadyExist, VCase} = lists:foldl(
                        fun(E, A) ->
                            {E, VLast, VTo, VLabel} = digraph:edge(G, E),
                            case VLabel =:= Label of
                                true -> {true, VTo};
                                false -> A
                            end
                        end,
                        {false, VLast},
                        EL
                    ),
                    case AlreadyExist of
                        true ->
                            {VCase, StateM};
                        false ->
                            digraph:add_edge(G, VLast, Vsecond, Label),
                            NewM = maps:put({{Proc1, V1}, {Proc2, PV1}}, VLast, StateM),
                            {Vsecond, NewM}
                    end
            end;
        _ ->
            io:fwrite("[ADD]First defined!!~n"),
            digraph:add_edge(G, VLast, Vfirst, Label),
            {Vfirst, StateM}
    end.

get_data_from_label(S) ->
    Ret = lists:nth(2, string:split(S, " ", all)),
    FirstChar = common_fun:first(Ret),
    if
        [FirstChar] =:= "[" -> Ret ++ " " ++ lists:nth(3, string:split(S, " ", all));
        true -> Ret
    end.

get_proc_from_label(S) -> lists:reverse(lists:nth(1, string:split(lists:reverse(S), " ", all))).

% stop all the processes
stop_processes(ProcMap) -> maps:foreach(fun(_K, V) -> V ! stop end, ProcMap).

atol(A) when is_list(A) -> A;
atol(A) when is_atom(A) -> atom_to_list(A).
ltoa(L) when is_atom(L) -> L;
ltoa(L) when is_list(L) -> list_to_atom(L).

get_proc_edges(P) -> send_recv(P, {self(), get_edges}).
get_proc_out_degree(P) -> send_recv(P, {self(), get_out_degree}).
get_proc_edge_info(P, E) -> send_recv(P, {self(), get_edge_info, E}).
get_proc_localvars(P) -> send_recv(P, {self(), get_local_vars}).
add_proc_localvars(P, V) -> P ! {add_local_var, V}.
get_proc_data(P) -> send_recv(P, {self(), get_data}).
set_proc_data(P, Data) -> P ! {set_data, Data}.
get_proc_mess_queue(P) -> send_recv(P, {self(), get_mess_queue}).
add_proc_mess_queue(P, M) -> P ! {add_mess_queue, M}.
del_proc_mess_queue(P, M) -> P ! {del_mess_queue, M}.

send_recv(P, Data) ->
    P ! Data,
    receive
        {D} -> D
    end.

proc_loop(Data) ->
    ProcName = Data#proc_info.proc_id,
    G = db_manager:get_fun_graph(ProcName),
    % timer:sleep(200),
    VCurr = Data#proc_info.current_vertex,
    FirstMarkedE = Data#proc_info.first_marked_edges,
    SecondMarkedE = Data#proc_info.second_marked_edges,
    MessageQueue = Data#proc_info.message_queue,
    LocalVars = Data#proc_info.local_vars,
    receive
        {use_transition, E} ->
            IsAlreadyMarkedOnce = lists:member(E, FirstMarkedE),
            case digraph:edge(G, E) of
                {E, VCurr, VNew, _} when IsAlreadyMarkedOnce ->
                    NewL =
                        case VNew =< VCurr of
                            true -> sets:new();
                            false -> LocalVars
                        end,
                    proc_loop(Data#proc_info{
                        current_vertex = VNew,
                        second_marked_edges = SecondMarkedE ++ [E],
                        local_vars = NewL
                    });
                {E, VCurr, VNew, _} ->
                    NewL =
                        case VNew =< VCurr of
                            true -> sets:new();
                            false -> LocalVars
                        end,
                    proc_loop(Data#proc_info{
                        current_vertex = VNew,
                        first_marked_edges = FirstMarkedE ++ [E],
                        local_vars = NewL
                    });
                _ ->
                    io:fwrite("[PROC LOOP] V ~p Edge ~p non trovato in ~p~n", [VCurr, E, ProcName]),
                    proc_loop(Data)
            end;
        {P, get_edges} ->
            EL = digraph:out_edges(G, VCurr),
            ERet = filter_marked_edges(EL, SecondMarkedE),
            P ! {ERet},
            proc_loop(Data);
        {P, get_out_degree} ->
            P ! {digraph:out_degree(G, VCurr)},
            proc_loop(Data);
        {P, get_edge_info, E} ->
            P ! {digraph:edge(G, E)},
            proc_loop(Data);
        {P, get_data} ->
            P ! {Data},
            proc_loop(Data);
        {set_data, NewData} ->
            proc_loop(NewData);
        {P, get_local_vars} ->
            P ! {sets:to_list(LocalVars)},
            proc_loop(Data);
        {add_local_var, V} ->
            proc_loop(Data#proc_info{local_vars = sets:add_element(V, LocalVars)});
        {P, get_mess_queue} ->
            P ! {MessageQueue},
            proc_loop(Data);
        {add_mess_queue, M} ->
            proc_loop(Data#proc_info{message_queue = MessageQueue ++ [M]});
        {del_mess_queue, M} ->
            proc_loop(Data#proc_info{message_queue = lists:delete(M, MessageQueue)});
        stop ->
            ok
    end.

filter_marked_edges(EdgeL, MarkedE) -> [E || E <- EdgeL, not lists:member(E, MarkedE)].
