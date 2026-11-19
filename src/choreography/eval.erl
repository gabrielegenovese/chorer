%%%-------------------------------------------------------------------
%%% @doc
%%% This module evaluate the AST of the expressions in the Erlang language.
%%% This module is usable only in the `lv' module.
%%% @end
%%%-------------------------------------------------------------------
-module(eval).
-include("../share/common_data.hrl").

%%% API
-export([
    function_list/2,
    clause/6,
    match/3,
    case_pm/3,
    if_pm/2,
    receive_pm/2,
    operation/4,
    function_call/3,
    anon_function/3,
    simple_type/3,
    atom/2,
    list/3,
    map/2,
    tuple/2,
    variable/2
]).

-type code_line() :: erl_parse:abstract_expr() |
                     erl_parse:abstract_form() |
                     erl_parse:abstract_clause() |
                     [erl_parse:abstract_expr()] |
                     [erl_parse:abstract_form()] |
                     [erl_parse:abstract_clause()].

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Evaluate the clauses of a function.
%%% The counter is used to eval in order the pattern matching.
-spec function_list(ContentList, Data) -> NewLVState when
      ContentList :: [erl_parse:abstract_clause()],
      Data :: #localview{},
      NewLVState :: #localview{}.
function_list(ContentList, Data) ->
    Param = Data#localview.param,
    element(
        1,
        lists:foldl(
            fun(FunctionBody, {AccData, Counter}) ->
                {clause, _, Pattern, Guard, Content} = FunctionBody,
                ets:insert(?ARGUMENTS, {Data#localview.fun_name, Pattern}),
                NewData = lv:eval_codeline(Pattern, AccData),
                {Cond, VarList} = check_matching(NewData#localview.ret_var#variable.value, Param),
                case Cond of
                    true ->
                        {
                            clause(
                                Content,
                                Pattern,
                                Guard,
                                %%% start the evaluation from the starting point
                                AccData#localview{last_vertex = 1, local_vars = VarList},
                                "arg",
                                Counter
                            ),
                            Counter + 1
                        };
                    false ->
                        {
                            clause(
                                Content,
                                Pattern,
                                Guard,
                                %%% start the evaluation from the starting point
                                AccData#localview{last_vertex = 1},
                                "arg",
                                Counter
                            ),
                            Counter + 1
                        }
                end
            end,
            {Data, 0},
            ContentList
        )
    ).

-spec check_matching(Pattern, Params) -> Response when
      Pattern :: [#variable{}],
      Params :: [#variable{}] | ?ANYDATA,
      Response :: {boolean(), [#variable{}] | []}.
check_matching(Pattern, Params) ->
    case share:unpack(Params) of
        ?ANYDATA ->
            {true, get_all_var(Pattern)};
        _ ->
            EnumPtmt = lists:enumerate(Pattern),
            EnumParam = lists:enumerate(Params),
            BoolList = [
                check_msg_comp(DataPtmt, DataMess)
             || {IndexPtmt, DataPtmt} <- EnumPtmt,
                {IndexMess, DataMess} <- EnumParam,
                IndexPtmt =:= IndexMess
            ],
            share:and_rec(BoolList)
    end.

-spec check_msg_comp(PatVar, ParVar) -> Response when
      PatVar :: #variable{},
      ParVar :: #variable{},
      Response :: {boolean(), [#variable{}] | []}.
check_msg_comp(PatVar, ParVar) ->
    PatType = PatVar#variable.type,
    ParType = ParVar#variable.type,
    PatValue = PatVar#variable.value,
    ParValue = ParVar#variable.value,
    case PatType of
        var ->
            {true, [PatVar#variable{type = ParType, value = ParValue}]};
        _ when PatType =:= ParType ->
            case PatValue =:= ParValue of
                true -> {true, []};
                false -> {false, []}
            end;
        _ ->
            {false, []}
    end.

-spec get_all_var(Pattern) -> VarList when
      Pattern :: [#variable{}],
      VarList :: [#variable{}].
get_all_var(Pattern) ->
    lists:filter(
        fun(P) ->
            case P#variable.type of
                var -> true;
                _ -> false
            end
        end,
        Pattern
    ).

%%% @doc
%%% Evaluate a single clause (might be from `case', `receive' or `if').
-spec clause(Code, Pattern, Guards, LVState, BaseLabel, Counter) -> NewLVState when
      Code :: [erl_parse:abstract_expr()],
      Pattern :: [erl_parse:abstract_expr()],
      Guards :: [[erl_parse:abstract_expr()]] | [],
      LVState :: #localview{},
      BaseLabel :: string(),
      Counter :: integer(),
      NewLVState :: #localview{}.
clause(Code, Pattern, Guards, Data, BaseLabel, Counter) ->
    % io:fwrite("[CLAUSE] Code ~p~n Vars ~p~n Guards ~p~n", [Code, Pattern, Guards]),
    LocalV = Data#localview.local_vars,
    TempData = lv:eval_codeline(Pattern, Data),
    % should always be a list
    EvalVarList = TempData#localview.ret_var,
    TempLabel =
        integer_to_list(Counter) ++ ?PMSEQSEP ++ BaseLabel ++ " " ++ var_to_string(EvalVarList) ++
            guards_to_string(Guards),
    FinalLabel = decide_label(BaseLabel, TempLabel),
    NewData = add_vertex_edge(FinalLabel, Data),
    NewVar = EvalVarList#variable.value,
    lists:foldl(
        fun(Line, AccData) -> lv:eval_codeline(Line, AccData) end,
        NewData#localview{local_vars = LocalV ++ NewVar},
        Code
    ).

%%% @doc
%%% Evaluate a variable declaration.
-spec match(RightContent, LeftContent, LVState) -> NewLVState when
      RightContent :: erl_parse:abstract_expr(),
      LeftContent :: erl_parse:abstract_expr(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
match(RightContent, LeftContent, Data) ->
    case RightContent of
        {var, _, VarName} -> match_with_var(VarName, LeftContent, Data);
        {tuple, _, VarList} -> match_with_tuple(VarList, LeftContent, Data);
        {cons, _, List} -> match_with_list(List, Data);
        R -> log:warning("LV", "[MATCH] couldn't understand line", R, Data, line)
    end.

%%% @doc
%%% Evaluate a `case' expression.
-spec case_pm(Content, PMList, LVState) -> NewLVState when
      Content :: erl_parse:abstract_expr(),
      PMList :: [erl_parse:abstract_clause()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
case_pm(Content, PMList, Data) ->
    NewData = lv:eval_codeline(Content, Data),
    %% TODO: save the returning variable?
    pattern_matching(PMList, "match", NewData).

%%% @doc
%%% Evaluate a `if' expression.
-spec if_pm(PMList, LVState) -> NewLVState when
      PMList :: [erl_parse:abstract_clause()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
if_pm(PMList, Data) ->
    pattern_matching(PMList, "if", Data).

%%% @doc
%%% Evaluate a `receive' expression.
-spec receive_pm(PMList, LVState) -> NewLVState when
      PMList :: [erl_parse:abstract_clause()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
receive_pm(PMList, Data) ->
    pattern_matching(PMList, "receive", Data).

%%% @doc
%%% Evaluate an operation.
-spec operation(Symbol, LeftContent, RightContent, LVState) -> NewLVState when
      Symbol :: atom(),
      RightContent :: erl_parse:abstract_expr(),
      LeftContent :: erl_parse:abstract_expr(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
operation(Symbol, LeftContent, RightContent, Data) ->
    case Symbol of
        '!' ->
            send(LeftContent, RightContent, Data);
        _ ->
            log:warning(
                "LV",
                "operation not yet implemented",
                Symbol,
                Data#localview{
                    ret_var = #variable{}
                },
                line
            )
    end.

%%% @doc
%%% Evaluate the call of a function.
-spec function_call(Function, ArgList, LVState) -> NewLVState when
      Function :: erl_parse:abstract_expr(),
      ArgList :: [erl_parse:abstract_expr()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
function_call(Function, ArgList, Data) ->
    case Function of
        {atom, _, Name} -> call_by_atom(Name, ArgList, Data);
        {var, _, VarName} -> call_by_var(VarName, ArgList, Data);
        {remote, _, Package, FunName} -> call_by_package(Package, FunName, ArgList, Data);
        F -> log:warning("LV", "couldn't recognize function call pattern", F, Data, line)
    end.

%%% @doc
%%% Evaluate the definition of an anonymous function.
%%% Naming convention: anon_LineNumber.SequentialNumber
-spec anon_function(Content, Line, LVState) -> NewLVState when
      Content :: {clauses, [erl_parse:abstract_clause()]},
      Line :: erl_anno:anno(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
anon_function(Content, Line, Data) ->
    case Content of
        {clauses, A} ->
            Id = "anonfun_" ++ integer_to_list(Line),
            ets:insert(?FUNAST, {Id, {function, Line, A}}),
            simple_type(function, Id, Data);
        _ ->
            log:warning("LV", "not recognized content in anon_function", Content, Data, line)
    end.

%%% @doc
%%% Evaluate a basic type.
-spec simple_type(Type, Val, LVState) -> NewLVState when
      Type :: atom(),
      Val :: any(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
simple_type(Type, Val, Data) ->
    VarRet = #variable{type = Type, value = Val},
    Data#localview{ret_var = VarRet}.

%%% @doc
%%% Evaluate an atom.
-spec atom(Val, LVState) -> NewLVState when
      Val :: atom(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
atom(Val, Data) ->
    %%% check if is a registerd atom, return the pid
    case ets:lookup(?REGISTERDB, Val) of
        [] -> simple_type(atom, Val, Data);
        [{_, Pid}] -> simple_type(pid, Pid, Data)
    end.

%%% @doc
%%% Evaluate a list.
-spec list(HeadList, TailList, LVState) -> NewLVState when
      HeadList :: erl_parse:abstract_expr(),
      TailList :: erl_parse:abstract_expr() | [erl_parse:abstract_expr()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
list(HeadList, TailList, Data) ->
    NDH = lv:eval_codeline(HeadList, Data),
    NDT = lv:eval_codeline(TailList, NDH),
    Var = NDH#localview.ret_var,
    VarList = NDT#localview.ret_var,
    NewVal = [Var] ++ VarList#variable.value,
    simple_type(list, NewVal, NDT).

%%% @doc
%%% Evaluate a map.
-spec map(Val, LVState) -> NewLVState when
      Val :: [erl_parse:abstract_expr()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
map(Val, Data) ->
    log:warning("LV", "TODO map evaluation", Val, Data, line).

%%% @doc
%%% Evaluate a tuple.
-spec tuple(Val, LVState) -> NewLVState when
      Val :: [erl_parse:abstract_expr()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
tuple(Val, Data) ->
    {NewVal, NewData} = lists:foldl(
        fun(I, {A, D}) ->
            ND = lv:eval_codeline(I, D),
            {A ++ [ND#localview.ret_var], ND}
        end,
        {[], Data},
        Val
    ),
    simple_type(tuple, NewVal, NewData).

%%% @doc
%%% Evaluate a variable.
-spec variable(VarName, LVState) -> NewLVState when
      VarName :: atom(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
variable(VarName, Data) ->
    Var = share:find_var(Data#localview.local_vars, VarName),
    RetV =
        case Var of
            not_found -> #variable{type = var, name = VarName};
            V -> V
        end,
    Data#localview{ret_var = RetV}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec decide_label(Base, Label) -> ReturnedLabel when
      Base :: string(),
      Label :: string(),
      ReturnedLabel :: string() | atom().
decide_label(Base, Label) ->
    MoreInfo = settings:get(more_info_lv),
    ToWriteL =
        if
            Base =:= "receive" ->
                {H, T} = remove_square_parenthesis(Label),
                H ++ T;
            Base =:= "arg" ->
                {H, T} = remove_square_parenthesis(Label),
                H ++ "(" ++ T ++ ")";
            MoreInfo ->
                %% to decide
                Label;
            true ->
                Label
        end,
    % for testing purposes add: or (Base =:= "arg"),
    Cond = (Base =:= "receive") orelse MoreInfo,
    share:get_base_label(Cond, ToWriteL).

-spec remove_square_parenthesis(Label) -> ModifiedLabel when
      Label :: string(),
      ModifiedLabel :: {string(), string()}.
remove_square_parenthesis(Label) ->
    [L | _] = string:split(Label, "]"),
    [H | [T]] = string:split(L, "["),
    {H, T}.

-spec match_with_var(VarName, LeftContent, LVState) -> NewLVState when
      VarName :: atom(),
      LeftContent :: code_line(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
match_with_var(VarName, LeftContent, Data) ->
    NewData = lv:eval_codeline(LeftContent, Data),
    Var = NewData#localview.ret_var,
    L = NewData#localview.local_vars,
    NewVarEntry = Var#variable{name = VarName},
    % io:fwrite("New var ~p~n", [NewVarEntry]),
    NewData#localview{ret_var = NewVarEntry, local_vars = L ++ [NewVarEntry]}.

-spec match_with_tuple(VarList, LeftContent, LVState) -> NewLVState when
      VarList :: list(),
      LeftContent :: code_line(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
match_with_tuple(VarList, LeftContent, Data) ->
    NewData = lv:eval_codeline(LeftContent, Data),
    Var = NewData#localview.ret_var,
    % not sure to use newdata
    L = NewData#localview.local_vars,
    case Var#variable.type of
        tuple ->
            {TupleListWithNames, _} =
                lists:foldl(
                    fun(Item, {AccL, [H | T]}) ->
                        {var, _, VarName} = H,
                        {AccL ++ [Item#variable{name = VarName}], T}
                    end,
                    {L, VarList},
                    Var#variable.value
                ),
            NewData#localview{
                ret_var = #variable{type = tuple, value = TupleListWithNames},
                local_vars = L ++ TupleListWithNames
            };
        _ ->
            RetVar = lists:foldl(fun(I, A) -> A ++ [#variable{name = I}] end, [], VarList),
            ND = NewData#localview{
                ret_var = #variable{type = tuple, value = RetVar},
                local_vars = L ++ RetVar
            },
            log:warning("LV", "right content is a tuple but left content is", Var, ND, line)
    end.

-spec match_with_list(List, LVState) -> NewLVState when
      List :: list(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
match_with_list(List, Data) ->
    log:warning("LV", "[MATCH] TODO match with list", List, Data, line).

-spec call_by_atom(Name, ArgList, LVState) -> NewLVState when
      Name :: atom(),
      ArgList :: [erl_parse:abstract_expr()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
call_by_atom(Name, ArgList, Data) ->
    FunName = Data#localview.fun_name,
    [RealNameS | _] = string:split(share:atol(FunName), ?ARITYSEP),
    RealName = share:ltoa(RealNameS),
    case Name of
        RealName -> recursive(ArgList, Data);
        spawn -> spawn_call(ArgList, Data);
        spawn_monitor -> spawn_monitor_call(ArgList, Data);
        self -> self_call(Data);
        register -> register_call(ArgList, Data);
        _ -> generic_call(Name, ArgList, Data)
    end.

-spec recursive(ArgList, LVState) -> NewLVState when
      ArgList :: [erl_parse:abstract_expr()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
recursive(ArgList, Data) ->
    % io:fwrite("[RECURSIVE] from vertex ~p~n", [Data#localview.last_vertex]),
    LV = Data#localview.last_vertex,
    EdgeMap = Data#localview.edge_additional_info,
    E = digraph:add_edge(Data#localview.graph, LV, 1, 'ɛ'),
    %%% TODO: add recursion eval in actor_emul
    NewEM = maps:put(E, {recursion, ArgList}, EdgeMap),
    % generic_call(share:remove_last(share:remove_last((Data#localview.fun_name))), ArgList, Data).
    %%% TODO: what to do with last_vertex? update with 1 or nothing? now it just lose all the information after the call
    % overapprox: return variable is unkwown
    % putting last_vertex = ?UNDEFINED the evaluation is stopped after an recursive call
    % Data#localview{ret_var = ?ANYDATA, edge_additional_info = NewEM}.
    Data#localview{last_vertex = ?UNDEFINED, ret_var = ?ANYDATA, edge_additional_info = NewEM}.

-spec spawn_call(ArgList, LVState) -> NewLVState when
      ArgList :: [erl_parse:abstract_expr()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
spawn_call(ArgList, Data) ->
    case ArgList of
        [Content] -> spawn_one(Content, Data);
        %%% TODO: check and implement package
        [_Package, {atom, _, Name}, SpArgList] -> spawn_three(Name, SpArgList, Data);
        %%% TODO: spawn with 2/4 argument
        _ -> log:warning("LV", "couldn't recognize spawn call pattern", ArgList, Data, line)
    end.

-spec spawn_one(Content, LVState) -> NewLVState when
      Content :: erl_parse:abstract_expr(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
spawn_one(Content, Data) ->
    NewData = lv:eval_codeline(Content, Data),
    VarFound = NewData#localview.ret_var,
    Id = VarFound#variable.value,
    lv:create_localview(Id, [], true),
    C = db:inc_spawn_counter(Id),
    S = Id ++ ?NSEQSEP ++ integer_to_list(C),
    RetData = add_vertex_edge("spawn " ++ S, NewData),
    RetVar = #variable{type = pid, value = S},
    RetData#localview{ret_var = RetVar}.

-spec spawn_three(Name, ArgList, LVState) -> NewLVState when
      Name :: atom(),
      ArgList :: erl_parse:abstract_expr(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
spawn_three(Name, ArgList, Data) ->
    NewData = lv:eval_codeline(ArgList, Data),
    NewDataRetVar = NewData#localview.ret_var,
    % io:fwrite("Rer var ~p~n", [NewDataRetVar]),
    {Label, ProcId} = format_spawn_label(Name, NewDataRetVar),
    EM = maps:put(Label, NewDataRetVar, NewData#localview.edge_additional_info),
    % io:fwrite("label ~p new edge map ~p map ~p~n", [Label, NewData#localview.fun_name, EM]),
    ND = NewData#localview{edge_additional_info = EM},
    RetData = add_vertex_edge(Label, ND),
    RetData#localview{ret_var = #variable{type = pid, value = ProcId}}.

%%% Naming convention for process: Name/Arity.SequentialNumber
%%% Naming convention for spawn label: spawn ProcName args ArgList
-spec format_spawn_label(Name, NewDataRetVar) -> SpawnLabel when
      Name :: atom(),
      NewDataRetVar :: #variable{},
      SpawnLabel :: {string(), string()}.
format_spawn_label(Name, NewDataRetVar) ->
    C = db:inc_spawn_counter(Name),
    Arity = length(NewDataRetVar#variable.value),
    ProcId = share:merge_fun_ar(Name, Arity) ++ ?NSEQSEP ++ integer_to_list(C),
    Label = "spawn " ++ ProcId ++ " args " ++ var_to_string(NewDataRetVar),
    {Label, ProcId}.

-spec spawn_monitor_call(ArgList, LVState) -> NewLVState when
      ArgList :: [erl_parse:abstract_expr()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
spawn_monitor_call(ArgList, Data) ->
    log:warning("LV", "spawn_monitor not yet implemented. Arguments =", ArgList, Data, line).

-spec self_call(LVState) -> NewLVState when
      LVState :: #localview{},
      NewLVState :: #localview{}.
self_call(Data) ->
    RetVar = #variable{type = pid, value = "pid_self"},
    Data#localview{ret_var = RetVar}.

%%% TODO: now the register function is implemented with a static behaviour,
%%% but should be dynamic.
-spec register_call(ArgList, LVState) -> NewLVState when
      ArgList :: [erl_parse:abstract_expr()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
register_call(ArgList, Data) ->
    [{atom, _, AtomName}, {var, _, VarName}] = ArgList,
    VarFound = share:find_var(Data, VarName),
    case VarFound of
        not_found ->
            ?UNDEFINED;
        V ->
            case V#variable.type of
                pid -> ets:insert(?REGISTERDB, {AtomName, V#variable.value});
                _ -> ?UNDEFINED
            end
    end,
    simple_type(atom, true, Data).

-spec generic_call(Name, ArgList, LVState) -> NewLVState when
      Name :: atom(),
      ArgList :: [erl_parse:abstract_expr()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
generic_call(Name, ArgList, Data) ->
    % TODO: how to evaluate argument list?
    NewData = lv:eval_codeline(ArgList, Data),
    NameString = share:merge_fun_ar(Name, length(ArgList)),
    case get_function_graph(NameString, NewData#localview.ret_var#variable.value) of
        no_graph ->
            log:warning(
                "LV", "couldn't parse function", Name, Data#localview{ret_var = #variable{}}, line
            );
        NewD ->
            G = Data#localview.graph,
            LastV = Data#localview.last_vertex,
            NewG = NewD#localview.graph,
            NewRet = NewD#localview.ret_var,
            EM = NewD#localview.edge_additional_info,
            NewLastV = merge_graph(G, NewG, LastV),
            % io:fwrite("LastV ~p VarRet ~p~n", [LastV, NewRet]),
            % io:fwrite("OldEM ~p NewEM ~p~n", [Data#localview.edge_additional_info, EM]),
            Data#localview{
                ret_var = NewRet,
                last_vertex = NewLastV,
                edge_additional_info = maps:merge(Data#localview.edge_additional_info, EM)
            }
    end.

-spec call_by_var(VarName, ArgList, LVState) -> NewLVState when
      VarName :: atom(),
      ArgList :: [erl_parse:abstract_expr()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
%%% TODO: eval argument list
call_by_var(VarName, ArgList, Data) ->
    G = Data#localview.graph,
    VLast = Data#localview.last_vertex,
    LocalVarL = Data#localview.local_vars,
    VarFound = share:find_var(LocalVarL, VarName),
    case VarFound of
        not_found ->
            log:warning("LV", "variable not found in call_by_var with name", VarName, Data, line);
        _ ->
            Id = VarFound#variable.value,
            %%% TODO: eval args
            _ND = lv:eval_codeline(ArgList, Data),
            NewData = lv:create_localview(Id, [], false),
            NewG = NewData#localview.graph,
            NewRet = NewData#localview.ret_var,
            NewLastV = merge_graph(G, NewG, VLast),
            Data#localview{ret_var = NewRet, last_vertex = NewLastV}
    end.

-spec call_by_package(Package, FunName, ArgList, LVState) -> NewLVState when
      Package :: erl_parse:abstract_expr(),
      FunName :: erl_parse:abstract_expr(),
      ArgList :: [erl_parse:abstract_expr()],
      LVState :: #localview{},
      NewLVState :: #localview{}.
call_by_package(Package, FunName, _ArgList, Data) ->
    {atom, _, Pack} = Package,
    {atom, _, Name} = FunName,
    case Pack of
        rand -> rand_package(Name, Data);
        %%% TODO: find the package and the function, create the local view of it, attach it to the current lv
        _ -> log:warning("LV", "package not yet implemented:", Pack, Data, line)
    end.

-spec rand_package(FunName, LVState) -> NewLVState when
      FunName :: atom(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
rand_package(FunName, Data) ->
    case FunName of
        uniform -> simple_type(integer, ?ANYDATA, Data);
        _ -> log:warning("LV", "rand's function not yet implemented:", FunName, Data, line)
    end.

-spec send(Destination, MessageContent, LVState) -> NewLVState when
      Destination :: erl_parse:abstract_expr(),
      MessageContent :: erl_parse:abstract_expr(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
send(Destination, MessageContent, Data) ->
    TempData = lv:eval_codeline(Destination, Data),
    VarProcName = TempData#localview.ret_var,
    ProcName = get_pid(VarProcName),
    % io:fwrite("Ret Var ~p found ~p~n", [VarProcName, ProcName]),
    NewData = lv:eval_codeline(MessageContent, TempData),
    VarDataSent = NewData#localview.ret_var,
    DataSent = var_to_string(VarDataSent),
    SLabel = format_send_local_label(ProcName, DataSent),
    EM = NewData#localview.edge_additional_info,
    add_vertex_edge(SLabel, NewData#localview{
        edge_additional_info = maps:put(SLabel, {VarProcName, VarDataSent}, EM)
    }).

-spec format_send_local_label(Proc, Data) -> string() when
      Proc :: atom() | string(),
      Data :: string().
format_send_local_label(Proc, Data) ->
    share:atol(Proc) ++ " " ++ ?SENDSEP ++ " " ++ share:atol(Data).

-spec get_pid(Var) -> Pid when
      Var :: #variable{},
      Pid :: string() | atom().
get_pid(Var) ->
    case Var#variable.type of
        pid ->
            Var#variable.value;
        _ ->
            case Var#variable.name of
                ?UNDEFINED -> Var#variable.type;
                _ -> share:atol(Var#variable.name)
            end
    end.

-spec var_to_string(Var) -> string() when
      Var :: [] | ?ANYDATA | #variable{}.
var_to_string(Var) ->
    case Var of
        [] ->
            "";
        ?ANYDATA ->
            "_";
        V ->
            Vname = V#variable.name,
            Vvalue = V#variable.value,
            Vtype = V#variable.type,
            case Vtype of
                ?ANYDATA ->
                    case Vname of
                        ?UNDEFINED -> share:atol(?ANYDATA);
                        _ -> share:atol(Vname)
                    end;
                Type ->
                    SType = share:atol(Type),
                    case Vvalue of
                        ?ANYDATA when Type =:= var -> share:atol(Vname);
                        ?ANYDATA ->
                            SType;
                        Val ->
                            case SType of
                                "integer" -> integer_to_list(Val);
                                "float" -> io_lib:format("~.2f", [Val]);
                                "string" -> "\'" ++ Val ++ "\'";
                                "atom" -> share:atol(Val);
                                "list" -> format_list(Val, fun var_to_string/1);
                                "tuple" -> format_tuple(Val, fun var_to_string/1);
                                "var" -> share:atol(Val);
                                "pid" -> share:atol(Val);
                                _ -> SType
                            end
                    end
            end
    end.

-spec format_tuple(VarL, Fun) -> string() when
      VarL :: [#variable{}],
      Fun :: function().
format_tuple(VarL, Fun) ->
    Label = join_list(VarL, Fun),
    "{" ++ Label ++ "}".

-spec format_list(VarL, Fun) -> string() when
      VarL :: [#variable{}],
      Fun :: function().
format_list(VarL, Fun) ->
    Label = join_list(VarL, Fun),
    "[" ++ Label ++ "]".

-spec join_list(VarL, Fun) -> string() when
      VarL :: [#variable{}],
      Fun :: function().
join_list(VarL, Fun) ->
    L = lists:foldl(fun(I, A) -> A ++ "," ++ Fun(I) end, "", VarL),
    case L of
        [] -> "";
        [_ | T] -> T
    end.

%%% We have a main graph G1, a graph G2 and a G1's vertex.
%%% We need to append G2 to the G1's vertex.
%%% Firstly, we add to G1 as many vertex as G2's vertex number.
%%% With a map, we associate every vertex of G2 to one of the new G1'vertex
%%% Secondly, we collect each G2's edge info and then we rebuild it in G1 using the previous map
-spec merge_graph(MainG, GToAdd, VLast) -> LastVAdded when
      MainG :: digraph:graph(),
      GToAdd :: digraph:graph(),
      VLast :: digraph:vertex(),
      LastVAdded :: digraph:vertex().
merge_graph(MainG, GToAdd, VLast) ->
    VertexGToAdd = digraph:vertices(GToAdd),
    % Add a number of vertex equal to VertexGToAdd
    VEquiMap = lists:foldl(
        %%% With this function we add a vertex and we associate it with G2's vertex
        fun(Item, M) -> maps:put(Item, share:add_vertex(MainG), M) end,
        maps:new(),
        VertexGToAdd
    ),
    % link the first new state created to the main graph
    digraph:add_edge(MainG, VLast, maps:get(1, VEquiMap), 'ɛ'),
    % Add all the edges
    EdgesGToAdd = digraph:edges(GToAdd),
    lists:foreach(
        fun(Item) ->
            {Item, V1, V2, Label} = digraph:edge(GToAdd, Item),
            %%% We use the map to rebuild the exact same edge but in G1
            digraph:add_edge(MainG, maps:get(V1, VEquiMap), maps:get(V2, VEquiMap), Label)
        end,
        EdgesGToAdd
    ),
    % return last added vertex, which is the max number in the key list
    maps:get(lists:max(maps:keys(VEquiMap)), VEquiMap).

-spec get_function_graph(FuncName, Arguments) -> Response when
      FuncName :: atom() | string(),
      Arguments :: atom() | list(),
      Response :: no_graph | #localview{}.
get_function_graph(FuncName, Arguments) ->
    FunAst = db:get_fun_ast(FuncName),
    case FunAst of
        not_found ->
            log:warning("LV", "No AST found for", FuncName, #localview{}),
            no_graph;
        _ ->
            lv:create_localview(FuncName, Arguments, false)
    end.

%%% Evaluate Pattern Matching's list of clauses: evaluate every branch alone, then
%%% link every last vertex's branch to a shared vertex with epsilon transaction
-spec pattern_matching(PMList, Label, LVState) -> NewLVState when
      PMList :: [erl_parse:abstract_clause()],
      Label :: string(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
pattern_matching(PMList, Label, Data) ->
    VLast = Data#localview.last_vertex,
    G = Data#localview.graph,
    VLastList = explore_pm(PMList, Label, Data),
    VRet = share:add_vertex(G),
    add_edges_recursive(G, VLastList, VRet, 'ɛ', VLast),
    Data#localview{graph = G, last_vertex = VRet}.

%%% Explore every pm's branch and returns the list of last added vertex
-spec explore_pm(PMList, Label, LVState) -> AddedVertexList when
      PMList :: [erl_parse:abstract_clause()],
      Label :: string(),
      LVState :: #localview{},
      AddedVertexList :: [digraph:vertex()].
explore_pm(PMList, Base, Data) ->
    element(
        1,
        lists:foldl(
            fun(CodeLine, {AddedVertexList, Couter}) ->
                case CodeLine of
                    {clause, _, Vars, Guard, Content} ->
                        VDataRet = clause(Content, Vars, Guard, Data, Base, Couter),
                        {AddedVertexList ++ [VDataRet#localview.last_vertex], Couter + 1};
                    C ->
                        log:warning(
                            "LV", "Should be clause but it's", C, {AddedVertexList, Couter}, line
                        )
                end
            end,
            {[], 0},
            PMList
        )
    ).

-spec add_vertex_edge(Label, LVState) -> NewLVState when
      Label :: string() | atom(),
      LVState :: #localview{},
      NewLVState :: #localview{}.
add_vertex_edge(Label, Data) ->
    G = Data#localview.graph,
    LastV = Data#localview.last_vertex,
    V = share:add_vertex(G),
    digraph:add_edge(G, LastV, V, Label),
    Data#localview{last_vertex = V}.

%%% Convert the guard (ast format) to a string
-spec guards_to_string(GlobalToVal) -> string() when
      GlobalToVal :: [[erl_parse:abstract_expr()]] | [].
guards_to_string(GlobalToVal) ->
    case GlobalToVal of
        %%% Add guards if there's a guard
        %%% TODO: add more infos
        {op, _, _, _} -> " (guards)";
        _ -> ""
    end.

%%% Link each vertex of a vertex's list to a given vertex, with a defined label
-spec add_edges_recursive(Graph, VertexList, VertexToLink, Label, Except) -> ok when
      Graph :: digraph:graph(),
      VertexList :: [digraph:vertex()],
      VertexToLink :: digraph:vertex(),
      Label :: string() | atom(),
      Except :: digraph:vertex().
add_edges_recursive(G, VertexList, VertexToLink, Label, Except) ->
    lists:foreach(
        fun(V) ->
            Cond = (V =/= Except) and (V =/= 1),
            case Cond of
                true -> digraph:add_edge(G, V, VertexToLink, Label);
                false -> done
            end
        end,
        VertexList
    ).
