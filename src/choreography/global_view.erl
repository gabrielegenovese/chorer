-module(global_view).
-include("../common/common_data.hrl").

%%% API
-export([generate/2, proc_loop/2]).

-record(branch, {graph, last_vertex, proc_pid_m, recv_l, send_l, states_m}).

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
    % MG = fsa:minimize(Gr),
    %%% buffo doppio minimize perche' uno solo non funziona
    % MG1 = fsa:minimize(MG),
    common_fun:save_graph_to_file(Gr, Dir, atol(EntryPoint), global).

create_globalview(Name) ->
    RetG = digraph:new(),
    VNew = common_fun:add_vertex(RetG),
    MainProcPid = spawn(?MODULE, proc_loop, [Name, 1]),
    ProcPidMap = #{Name => MainProcPid},
    %%% initialize data structures
    progress_proc(RetG, [new_branch(RetG, VNew, ProcPidMap, [], [], #{})]).

new_branch(G, V, P, R, S, M) ->
    #branch{
        graph = G,
        last_vertex = V,
        proc_pid_m = P,
        recv_l = R,
        send_l = S,
        states_m = M
    }.

progress_proc(G, []) ->
    G;
progress_proc(GlobalGraph, BranchList) when is_list(BranchList) ->
    io:fwrite("Branch to eval ~p~n", [length(BranchList)]),
    NewBL = lists:foldl(
        fun(Item, AccL) ->
            % io:fwrite("Eval branch~n"),
            NewBreanches = progress_single_branch(Item),
            AccL ++ NewBreanches
        end,
        [],
        BranchList
    ),
    progress_proc(GlobalGraph, NewBL).

progress_single_branch(BData) ->
    progress_single_branch(BData, []).

progress_single_branch(BData, AccL) ->
    {NewBData, NBL, Op1} = maps:fold(
        fun(Name, Pid, AccData) -> eval_proc_until_send(Name, Pid, AccData) end,
        {BData, [], false},
        BData#branch.proc_pid_m
    ),
    SendList = maps:fold(
        fun(Name, Pid, AL) -> AL ++ get_possible_send(Name, Pid) end,
        [],
        NewBData#branch.proc_pid_m
    ),
    % io:fwrite("SendList ~p~n", [SendList]),
    case SendList =:= [] of
        true ->
            case Op1 of
                true -> progress_single_branch(NewBData, AccL ++ NBL);
                false -> AccL
            end;
        false ->
            {NL, Modified} = lists:foldl(
                fun(I, A) ->
                    % io:fwrite("Eval ~p~n", [I]),
                    {L, O} = A,
                    {PName, SLabel, E} = I,
                    {ND, Op} = manage_send(SLabel, dup_branch(NewBData), PName, E),
                    {L ++ [ND], O or Op}
                end,
                {[], false},
                SendList
            ),
            stop_processes(NewBData#branch.proc_pid_m),
            case (NL =:= []) and (not Modified) of
                true ->
                    AccL;
                false ->
                    [H | T] = NL,
                    progress_single_branch(H, AccL ++ NBL ++ T)
            end
    end.

dup_branch(Data) ->
    Data#branch{proc_pid_m = duplicate_proccess(Data#branch.proc_pid_m)}.

duplicate_proccess(ProcMap) ->
    maps:fold(
        fun(K, V, A) ->
            {Name, _} = remove_last_with_check(K),
            NewPid = spawn(?MODULE, proc_loop, [Name, 1]),
            set_proc_data(NewPid, get_proc_data(V)),
            maps:put(K, NewPid, A)
        end,
        #{},
        ProcMap
    ).

remove_last_with_check(ProcId) ->
    {Name, N} = remove_last(atol(ProcId)),
    case catch list_to_integer(N) of
        {'EXIT', _} -> {ProcId, N};
        _ -> {ltoa(Name), N}
    end.

eval_proc_until_send(ProcName, ProcPid, AccData) ->
    ProcOD = get_proc_out_degree(ProcPid),
    {Data, NBL, Bool} = AccData,
    EL = get_proc_edges(ProcPid),
    {NewData, NL, OpDone} =
        if
            %%% TODO: verify this -> No out edges equals to final state?
            ProcOD =:= 0 ->
                {Data, [], false};
            ProcOD =:= 1 ->
                EToEval = choose_edge(ProcPid),
                {D, O} = eval_edge(EToEval, ProcName, ProcPid, Data),
                {D, [], O};
            true ->
                Cond = is_lists_edgerecv(ProcPid, EL),
                case Cond of
                    true ->
                        % io:fwrite("RecvList ~p~n", [EL]),
                        {DataRet, _} = lists:foldl(
                            fun(E, A) ->
                                {DataRecv, MatchFound} = A,
                                case MatchFound of
                                    true ->
                                        A;
                                    false ->
                                        {B, _, C} = DataRecv,
                                        EInfo = get_proc_edge_info(ProcPid, E),
                                        {D, O, NewMatch} = manage_recv(B, ProcName, ProcPid, EInfo),
                                        {{D, [], C or O}, NewMatch}
                                end
                            end,
                            {{Data, [], false}, false},
                            EL
                        ),
                        DataRet;
                    %%% TODO da testare, non ci entra mai il programma qua
                    false ->
                        {NewL, NOp} = lists:foldl(
                            fun(E, A) ->
                                EInfo = get_proc_edge_info(ProcPid, E),
                                {B, C} = A,
                                {D, O} = eval_edge(EInfo, ProcName, ProcPid, Data),
                                {B ++ [D], C or O}
                            end,
                            {[], false},
                            EL
                        ),
                        [H | T] = NewL,
                        {H, T, NOp}
                end
        end,
    case OpDone of
        false -> {NewData, NBL, Bool};
        true -> eval_proc_until_send(ProcName, ProcPid, {NewData, NBL ++ NL, true})
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

get_possible_send(ProcName, ProcPid) ->
    ProcOD = get_proc_out_degree(ProcPid),
    if
        ProcOD =:= 0 ->
            [];
        true ->
            EL = get_proc_edges(ProcPid),
            % io:fwrite("EdgeList ~p for ~p~n", [EL, ProcName]),
            lists:foldl(
                fun(E, A) ->
                    EInfo = get_proc_edge_info(ProcPid, E),
                    {_, _, _, PLabel} = EInfo,
                    SLabel = atol(PLabel),
                    IsSend = string:find(SLabel, "send"),
                    A ++
                        case is_list(IsSend) of
                            true -> [{ProcName, SLabel, EInfo}];
                            false -> []
                        end
                end,
                [],
                EL
            )
    end.

choose_edge(ProcPid) ->
    EL = get_proc_edges(ProcPid),
    %%% pick an edge
    E = common_fun:first(EL),
    get_proc_edge_info(ProcPid, E).

eval_edge(EdgeInfo, ProcName, ProcPid, BData) ->
    {Edge, _, _, PLabel} = EdgeInfo,
    io:fwrite("Proc ~p eval label ~p~n", [ProcName, PLabel]),
    SLabel = atol(PLabel),
    IsArg = string:find(SLabel, "arg"),
    IsSpawn = string:find(SLabel, "spawn"),
    IsReceive = string:find(SLabel, "receive"),
    if
        is_list(IsArg) ->
            ProcPid ! {use_transition, Edge},
            {BData, true};
        is_list(IsSpawn) ->
            {VNew, NewM} = add_spawn_to_global(SLabel, ProcName, BData),
            ProcPid ! {use_transition, Edge},
            NewBData = BData#branch{last_vertex = VNew, proc_pid_m = NewM},
            {NewBData, true};
        %% todo: capire se utile
        is_list(IsReceive) ->
            {D, O, _} = manage_recv(BData, ProcName, ProcPid, EdgeInfo),
            {D, O};
        true ->
            {BData, false}
    end.

add_spawn_to_global(SLabel, ProcName, Data) ->
    CompleteProcNameS = string:prefix(SLabel, "spawn "),
    {FuncName, _ProcNumber} = remove_last(CompleteProcNameS),
    FuncPid = spawn(?MODULE, proc_loop, [ltoa(FuncName), 1]),
    NewMap = maps:put(ltoa(CompleteProcNameS), FuncPid, Data#branch.proc_pid_m),
    VNew = common_fun:add_vertex(Data#branch.graph),
    %%% Δ means spawned
    NewLabel = atol(ProcName) ++ "Δ" ++ CompleteProcNameS,
    digraph:add_edge(Data#branch.graph, Data#branch.last_vertex, VNew, NewLabel),
    {VNew, NewMap}.

remove_last(A) -> lists:split(length(A) - 1, A).

manage_send(SLabel, Data, ProcName, EdgeInfo) ->
    {Edge, _, _, _} = EdgeInfo,
    ProcPid = maps:get(ProcName, Data#branch.proc_pid_m),
    ProcPid ! {use_transition, Edge},
    DataSent = get_data_from_label(SLabel),
    ProcSent = ltoa(get_proc_from_label(SLabel)),
    SendL = Data#branch.send_l,
    RecvL = Data#branch.recv_l,
    PL = find_compatibility(Data#branch.proc_pid_m, ProcName, RecvL, ProcSent, DataSent, send),
    if
        PL =:= [] ->
            AlreadyMember = lists:member({ProcName, Edge, ProcSent, DataSent}, SendL),
            NewSL = SendL ++ [{ProcName, Edge, ProcSent, DataSent}],
            case AlreadyMember of
                true -> {Data, false};
                false -> {Data#branch{send_l = NewSL}, true}
            end;
        PL =/= [] ->
            % pick a receiver
            Entry = common_fun:first(PL),
            {ProcNRecv, ProcRecvE, _, _} = Entry,
            PPid = maps:get(ProcNRecv, Data#branch.proc_pid_m),
            E2Info = get_proc_edge_info(PPid, ProcRecvE),
            NewL = atol(ProcName) ++ "→" ++ atol(ProcNRecv) ++ ":" ++ DataSent,
            {VNA, NewSMap} = decide_vertex(ProcName, EdgeInfo, ProcNRecv, E2Info, Data, NewL),
            PPid ! {use_transition, ProcRecvE},
            NewRecvL = delete_recv(lists:delete(Entry, RecvL), ProcNRecv),
            {Data#branch{last_vertex = VNA, recv_l = NewRecvL, states_m = NewSMap}, true}
    end.

manage_recv(Data, ProcName, ProcPid, EdgeInfo) ->
    {Edge, _, _, PLabel} = EdgeInfo,
    SLabel = atol(PLabel),
    DataRecv = get_data_from_label(SLabel),
    RecvL = Data#branch.recv_l,
    SendL = Data#branch.send_l,
    CompatibleRv = find_compatibility(Data#branch.proc_pid_m, SendL, ProcName, DataRecv, recv),
    case CompatibleRv =:= [] of
        true ->
            AlreadyMember = lists:member({ProcName, Edge, ProcName, DataRecv}, RecvL),
            NewRL = RecvL ++ [{ProcName, Edge, ProcName, DataRecv}],
            case AlreadyMember of
                true -> {Data, false, false};
                false -> {Data#branch{recv_l = NewRL}, true, false}
            end;
        false ->
            % pick a sender
            E = common_fun:first(CompatibleRv),
            % io:fwrite("E chose ~p~n", [E]),
            {ProcSent, ProcSentE, _, DSent} = E,
            PPid = maps:get(ProcSent, Data#branch.proc_pid_m),
            E2Info = get_proc_edge_info(PPid, ProcSentE),
            NewL = atol(ProcSent) ++ "→" ++ atol(ProcName) ++ ":" ++ DSent,
            {VNA, NewSM} = decide_vertex(ProcName, EdgeInfo, ProcSent, E2Info, Data, NewL),
            NewRecvL = delete_recv(RecvL, ProcName),
            ProcPid ! {use_transition, Edge},
            {
                Data#branch{
                    last_vertex = VNA,
                    recv_l = NewRecvL,
                    send_l = lists:delete(E, SendL),
                    states_m = NewSM
                },
                true,
                true
            }
    end.

delete_recv(RecvL, ProcName) ->
    lists:filter(
        fun(El) ->
            {Name, _, _, _} = El,
            ProcName =/= Name
        end,
        RecvL
    ).

find_compatibility(ProcPidM, SendL, Name, Message, recv) ->
    lists:foldl(
        fun(I, A) ->
            {PName, _, ProcSent, Data} = I,
            PPid = maps:get(PName, ProcPidM),
            NewPName = check_vars(PPid, PName, ProcSent),
            Cond =
                is_proc_compatible(Name, NewPName) and is_message_compatible(PPid, Message, Data),
            case Cond of
                true -> A ++ [I];
                false -> A
            end
        end,
        [],
        SendL
    ).

find_compatibility(ProcPidM, ProcId, RecvL, Name, Message, send) ->
    PPid = maps:get(ProcId, ProcPidM),
    NewPName = check_vars(PPid, ProcId, Name),
    [
        {PName, E, ProcSent, Data}
     || {PName, E, ProcSent, Data} <- RecvL,
        is_proc_compatible(ProcSent, NewPName),
        is_message_compatible(PPid, Data, Message)
    ].

find_spawn_info(PId) ->
    SpInfoAll = db_manager:get_spawn_info(),
    common_fun:first(lists:filter(fun(S) -> S#spawned_proc.name =:= PId end, SpInfoAll)).

check_vars(ProcPid, PId, VarName) ->
    SpInfoP = find_spawn_info(PId),
    LVars = get_proc_local(ProcPid),
    {Name, _} = remove_last_with_check(PId),
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
    io:fwrite("Find var ~p in ~p~n", [VarName, SeachList]),
    VarValue = find_var(SeachList, VarName),
    case VarValue of
        nomatch ->
            VarName;
        V ->
            case V#variable.type of
                ?UNDEFINED -> VarName;
                "pid_self" -> SpInfoP#spawned_proc.called_where;
                pid_self -> SpInfoP#spawned_proc.called_where;
                _ -> V#variable.type
            end
    end.

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

is_proc_compatible(PSent, PName) ->
    PSent =:= PName.

is_message_compatible(ProcPid, PatternMatching, Message) ->
    MessageS = atol(Message),
    PatternMS = atol(PatternMatching),
    [FirstPChar | RestP] = PatternMS,
    [FirstMChar | RestM] = MessageS,
    Cond = is_uppercase([FirstPChar]),
    if
        %%% hierarchy
        [FirstPChar] =:= "_" ->
            true;
        ([FirstPChar] =:= "{") and ([FirstMChar] =:= "{") ->
            {ContentP, _} = remove_last(RestP),
            {ContentM, _} = remove_last(RestM),
            PL = string:split(ContentP, ",", all),
            A = lists:enumerate(PL),
            ML = string:split(ContentM, ",", all),
            B = lists:enumerate(ML),
            Bool = [
                is_message_compatible(ProcPid, IA, IB)
             || {BA, IA} <- A, {BB, IB} <- B, BA =:= BB
            ],
            and_rec(Bool);
        PatternMS =:= MessageS ->
            true;
        Cond ->
            register_var(ProcPid, PatternMatching, Message),
            true;
        true ->
            false
    end.

register_var(ProcPid, Name, Type) ->
    IsPid = string:prefix(Type, "pid_"),
    [FC | _] = Type,
    IsLower = is_lowercase([FC]),
    TVal =
        if
            is_list(IsPid) -> ltoa(Type);
            IsLower -> atom;
            true -> ltoa(Type)
        end,
    V = #variable{name = ltoa(Name), type = TVal},
    % io:fwrite("Added Var ~p~n", [V]),
    add_proc_local(ProcPid, V).

and_rec([]) -> true;
and_rec([H | T]) -> H and and_rec(T).

is_uppercase(FirstChar) when
    (is_list(FirstChar)) and (length(FirstChar) =:= 1)
->
    (FirstChar >= "A") and (FirstChar =< "Z").

is_lowercase(FirstChar) when
    (is_list(FirstChar)) and (length(FirstChar) =:= 1)
->
    (FirstChar >= "a") and (FirstChar =< "z").

decide_vertex(Proc1, Edge1, Proc2, Edge2, Data, Label) ->
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
                    digraph:add_edge(G, VLast, Vsecond, Label),
                    {Vsecond, StateM}
            end;
        _ ->
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
get_proc_local(P) -> send_recv(P, {self(), get_local_vars}).
add_proc_local(P, V) -> P ! {add_local_var, V}.
get_proc_data(P) -> send_recv(P, {self(), get_data}).
set_proc_data(P, Data) -> P ! {set_data, Data}.

send_recv(P, Data) ->
    P ! Data,
    receive
        {D} -> D
    end.

proc_loop(ProcName, VCurrent) ->
    proc_loop(ProcName, VCurrent, [], [], []).
proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE, LocalVars) ->
    G = db_manager:get_fun_graph(ProcName),
    % timer:sleep(200),
    receive
        {use_transition, E} ->
            IsAlreadyMarkedOnce = lists:member(E, FirstMarkedE),
            case digraph:edge(G, E) of
                {E, VCurrent, VNew, _} when IsAlreadyMarkedOnce ->
                    proc_loop(ProcName, VNew, FirstMarkedE, SecondMarkedE ++ [E], LocalVars);
                {E, VCurrent, VNew, _} ->
                    proc_loop(ProcName, VNew, FirstMarkedE ++ [E], SecondMarkedE, LocalVars);
                _ ->
                    % io:fwrite("V ~p edge ~p non trovato in ~p~n", [VCurrent, E, ProcName]),
                    proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE, LocalVars)
            end;
        {P, get_edges} ->
            EL = digraph:out_edges(G, VCurrent),
            ERet = filter_marked_edges(EL, SecondMarkedE),
            P ! {ERet},
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE, LocalVars);
        {P, get_out_degree} ->
            P ! {digraph:out_degree(G, VCurrent)},
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE, LocalVars);
        {P, get_edge_info, E} ->
            P ! {digraph:edge(G, E)},
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE, LocalVars);
        {P, get_data} ->
            P ! {{VCurrent, FirstMarkedE, SecondMarkedE}},
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE, LocalVars);
        {set_data, {V, FE, SE}} ->
            proc_loop(ProcName, V, FE, SE, LocalVars);
        {P, get_local_vars} ->
            P ! {LocalVars},
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE, LocalVars);
        {add_local_var, V} ->
            proc_loop(ProcName, VCurrent, FirstMarkedE, SecondMarkedE, LocalVars ++ [V]);
        stop ->
            ok
    end.

filter_marked_edges(EdgeL, MarkedE) -> [E || E <- EdgeL, not lists:member(E, MarkedE)].
