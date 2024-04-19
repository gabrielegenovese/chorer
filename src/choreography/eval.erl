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

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Evaluate the clauses of a function.
function_list(ContList, Data) ->
    element(
        1,
        lists:foldl(
            fun(FunctionBody, {AccData, Counter}) ->
                case FunctionBody of
                    {clause, _, Pattern, Guard, Content} ->
                        % io:fwrite("Clause ~p~n", [Vars]),
                        ets:insert(?ARGUMENTS, {Data#localview.fun_name, Pattern}),
                        {
                            clause(
                                Content,
                                Pattern,
                                Guard,
                                AccData#localview{last_vertex = 1},
                                "arg",
                                Counter
                            ),
                            Counter + 1
                        };
                    C ->
                        share:warning("should be a clause but it's", C, {?UNDEFINED, Counter + 1})
                end
            end,
            {Data, 0},
            ContList
        )
    ).

%%% @doc
%%% Evaluate a single clause (might be from `case', `receive' or `if').
clause(Code, Pattern, Guards, Data, BaseLabel, Counter) ->
    % io:fwrite("[CLAUSE] Code ~p~n Vars ~p~n Guards ~p~n", [Code, Pattern, Guards]),
    LocalV = Data#localview.local_vars,
    TempData = lv:eval_codeline(Pattern, Data),
    % should always be a list
    EvalVarList = TempData#localview.ret_var,
    TempLabel =
        integer_to_list(Counter) ++ ?PMSEQSEP ++ BaseLabel ++ " " ++ var_to_string(EvalVarList) ++
            guards_to_string(Guards),
    FinalLabel = decide_label(BaseLabel, TempLabel, Data),
    NewData = add_vertex_edge(FinalLabel, Data),
    lists:foldl(
        fun(Line, AccData) -> lv:eval_codeline(Line, AccData) end,
        NewData#localview{local_vars = LocalV ++ EvalVarList#variable.value},
        Code
    ).

%%% @doc
%%% Evaluate a variable declaration.
match(RightContent, LeftContent, Data) ->
    case RightContent of
        {var, _, VarName} -> match_with_var(VarName, LeftContent, Data);
        {tuple, _, VarList} -> match_with_tuple(VarList, LeftContent, Data);
        {cons, _, List} -> match_with_list(List, Data);
        R -> share:warning("[MATCH] couldn't understand line", R, Data)
    end.

%%% @doc
%%% Evaluate a `case' expression.
case_pm(Content, PMList, Data) ->
    NewData = lv:eval_codeline(Content, Data),
    %% TODO: save the returning variable?
    pattern_matching(PMList, "match", NewData).

%%% @doc
%%% Evaluate a `if' expression.
if_pm(PMList, Data) ->
    pattern_matching(PMList, "if", Data).

%%% @doc
%%% Evaluate a `receive' expression.
receive_pm(PMList, Data) ->
    pattern_matching(PMList, "receive", Data).

%%% @doc
%%% Evaluate an operation.
operation(Symbol, LeftContent, RightContent, Data) ->
    case Symbol of
        '!' -> send(LeftContent, RightContent, Data);
        %%% TODO: implement other basic operation
        _ -> share:warning("operation not yet implemented", Symbol, Data)
    end.

%%% @doc
%%% Evaluate the call of a function.
function_call(Function, ArgList, Data) ->
    case Function of
        {atom, _, Name} -> call_by_atom(Name, ArgList, Data);
        {var, _, VarName} -> call_by_var(VarName, ArgList, Data);
        {remote, _, Package, FunName} -> call_by_package(Package, FunName, ArgList, Data);
        F -> share:warning("couldn't recognize function call pattern", F, Data)
    end.

%%% @doc
%%% Evaluate the definition of an anonymous function.
%%% Naming convention: anon_LineNumber.SequentialNumber
anon_function(Content, Line, Data) ->
    case Content of
        {clauses, A} ->
            Id = "anonfun_" ++ integer_to_list(Line),
            ets:insert(?FUNAST, {Id, {function, Line, A}}),
            simple_type(function, Id, Data);
        _ ->
            share:warning("not recognized content in anon_function", Content, Data)
    end.

%%% @doc
%%% Evaluate a basic type.
simple_type(Type, Val, Data) ->
    VarRet = #variable{type = Type, value = Val},
    Data#localview{ret_var = VarRet}.

%%% @doc
%%% Evaluate an atom.
atom(Val, Data) ->
    %%% check if is a registerd atom, return the pid
    case ets:lookup(?REGISTERDB, Val) of
        [] -> simple_type(atom, Val, Data);
        [{_, Pid}] -> simple_type(pid, Pid, Data)
    end.

%%% @doc
%%% Evaluate a list.
list(HeadList, TailList, Data) ->
    NDH = lv:eval_codeline(HeadList, Data),
    NDT = lv:eval_codeline(TailList, NDH),
    Var = NDH#localview.ret_var,
    VarList = NDT#localview.ret_var,
    NewVal = [Var] ++ VarList#variable.value,
    simple_type(list, NewVal, NDT).

%%% @doc
%%% Evaluate a map. TODO
map(Val, Data) ->
    share:warning("TODO map evaluation", Val, Data).

%%% @doc
%%% Evaluate a tuple.
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
variable(VarName, Data) ->
    Var = share:find_var(Data#localview.local_vars, VarName),
    RetV =
        case Var of
            not_found -> #variable{name = VarName};
            V -> V
        end,
    Data#localview{ret_var = RetV}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

decide_label(Base, Label, Data) ->
    Settings = Data#localview.settings,
    MoreInfo = Settings#setting.more_info_lv,
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
    Cond = (Base =:= "receive") or MoreInfo,
    share:get_base_label(Cond, ToWriteL).

remove_square_parenthesis(Label) ->
    [L | _] = string:split(Label, "]"),
    [H | [T]] = string:split(L, "["),
    {H, T}.

match_with_var(VarName, LeftContent, Data) ->
    NewData = lv:eval_codeline(LeftContent, Data),
    Var = NewData#localview.ret_var,
    L = NewData#localview.local_vars,
    NewVarEntry = Var#variable{name = VarName},
    % io:fwrite("New var ~p~n", [NewVarEntry]),
    NewData#localview{ret_var = NewVarEntry, local_vars = L ++ [NewVarEntry]}.

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
            share:warning("right content is a tuple but left content is", Var, ND)
    end.

match_with_list(List, Data) ->
    share:warning("[MATCH] TODO match with list", List, Data).

call_by_atom(Name, ArgList, Data) ->
    FunName = Data#localview.fun_name,
    %%% TODO: change this line to be generic (split with ?ARITYSEP)
    RealName = share:ltoa(share:remove_last(share:remove_last(FunName))),
    case Name of
        RealName -> recursive(ArgList, Data);
        spawn -> spawn_call(ArgList, Data);
        spawn_monitor -> spawn_monitor_call(ArgList, Data);
        self -> self_call(Data);
        register -> register_call(ArgList, Data);
        _ -> generic_call(Name, ArgList, Data)
    end.

%%% TODO: What to do with argument lists? Put them in the #localview.edge_additional_info?
recursive(_ArgList, Data) ->
    % io:fwrite("[RECURSIVE] from vertex ~p~n", [Data#localview.last_vertex]),
    digraph:add_edge(Data#localview.graph, Data#localview.last_vertex, 1, 'ɛ'),
    Data#localview{last_vertex = ?UNDEFINED}.

spawn_call(ArgList, Data) ->
    case ArgList of
        [Content] -> spawn_one(Content, Data);
        %%% TODO: check and implement package
        [_Package, {atom, _, Name}, SpArgList] -> spawn_three(Name, SpArgList, Data);
        %%% TODO: spawn with 2/4 argument
        _ -> share:warning("couldn't recognize spawn call pattern", ArgList, Data)
    end.

spawn_one(Content, Data) ->
    NewData = lv:eval_codeline(Content, Data),
    VarFound = NewData#localview.ret_var,
    Id = VarFound#variable.value,
    lv:create_localview(Id, Data#localview.settings, true),
    C = share:inc_spawn_counter(Id),
    S = Id ++ ?NSEQSEP ++ integer_to_list(C),
    RetData = add_vertex_edge("spawn " ++ S, NewData),
    RetVar = #variable{type = pid, value = S},
    RetData#localview{ret_var = RetVar}.

spawn_three(Name, ArgList, Data) ->
    NewData = lv:eval_codeline(ArgList, Data),
    NewDataRetVar = NewData#localview.ret_var,
    {Label, ProcId} = format_spawn_label(Name, NewDataRetVar),
    EM = maps:put(Label, NewDataRetVar, NewData#localview.edge_additional_info),
    % io:fwrite("label ~p new edge map ~p map ~p~n", [Label, NewData#localview.fun_name, EM]),
    ND = NewData#localview{edge_additional_info = EM},
    RetData = add_vertex_edge(Label, ND),
    RetData#localview{ret_var = #variable{type = pid, value = ProcId}}.

%%% Naming convention: Name/Arity.SequentialNumber
format_spawn_label(Name, NewDataRetVar) ->
    C = share:inc_spawn_counter(Name),
    Arity = length(NewDataRetVar#variable.value),
    ProcId = share:merge_fun_ar(Name, Arity) ++ ?NSEQSEP ++ integer_to_list(C),
    {"spawn " ++ ProcId, ProcId}.

spawn_monitor_call(ArgList, Data) ->
    share:warning("spawn_monitor not yet implemted. Arguments =", ArgList, Data).

self_call(Data) ->
    RetVar = #variable{type = pid, value = "pid_self"},
    Data#localview{ret_var = RetVar}.

%%% TODO: now the register function is implemented with a static behaviour,
%%% but should be dynamic.
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
    %%% ignoring returing value
    Data.

generic_call(Name, ArgList, Data) ->
    % TODO: how to evaluate argument list?
    _NewData = lv:eval_codeline(ArgList, Data),
    % io:fwrite("ARG LIST ~p~n", [ArgList]),
    % io:fwrite("RET VAR ~p~n", [NewData#localview.ret_var#variable.value]),
    NameString = share:merge_fun_ar(Name, length(ArgList)),
    case get_function_graph(NameString, Data#localview.settings) of
        no_graph ->
            share:warning("couldn't parse function", Name, Data#localview{ret_var = #variable{}});
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

%%% TODO: eval argument list
call_by_var(VarName, ArgList, Data) ->
    G = Data#localview.graph,
    VLast = Data#localview.last_vertex,
    LocalVarL = Data#localview.local_vars,
    VarFound = share:find_var(LocalVarL, VarName),
    case VarFound of
        not_found ->
            share:warning("variable not found in call_by_var with name", VarName, Data);
        _ ->
            Id = VarFound#variable.value,
            ND = lv:eval_codeline(ArgList, Data),
            NewData = lv:create_localview(Id, ND#localview.settings, false),
            NewG = NewData#localview.graph,
            NewRet = NewData#localview.ret_var,
            NewLastV = merge_graph(G, NewG, VLast),
            Data#localview{ret_var = NewRet, last_vertex = NewLastV}
    end.

call_by_package(Package, FunName, _ArgList, Data) ->
    {atom, _, Pack} = Package,
    {atom, _, Name} = FunName,
    case Pack of
        rand -> rand_package(Name, Data);
        %%% TODO: find the package and the function, create the local view of it, attach it to the current lv
        _ -> share:warning("package not yet implemented:", Pack, Data)
    end.

rand_package(FunName, Data) ->
    case FunName of
        uniform -> simple_type(integer, ?ANYDATA, Data);
        _ -> share:warning("rand's function not yet implemented:", FunName, Data)
    end.

send(Destination, MessageContent, Data) ->
    TempData = lv:eval_codeline(Destination, Data),
    VarProcName = TempData#localview.ret_var,
    ProcName = get_pid(VarProcName),
    % io:fwrite("Ret Var ~p found ~p~n", [VarProcName, ProcName]),
    NewData = lv:eval_codeline(MessageContent, TempData),
    VarDataSent = NewData#localview.ret_var,
    DataSent = var_to_string(VarDataSent),
    SLabel = "send " ++ share:atol(DataSent) ++ " to " ++ share:atol(ProcName),
    EM = NewData#localview.edge_additional_info,
    add_vertex_edge(SLabel, NewData#localview{
        edge_additional_info = maps:put(SLabel, {VarProcName, VarDataSent}, EM)
    }).

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

var_to_string(Var) ->
    case Var of
        [] ->
            "";
        V ->
            case V#variable.type of
                ?ANYDATA ->
                    share:atol(V#variable.name);
                Type ->
                    SType = share:atol(Type),
                    case V#variable.value of
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
                                "pid" -> share:atol(V#variable.value);
                                S -> share:atol(S)
                            end
                    end
            end
    end.

format_tuple(VarL, Fun) ->
    Label = join_list(VarL, Fun),
    "{" ++ Label ++ "}".

format_list(VarL, Fun) ->
    Label = join_list(VarL, Fun),
    "[" ++ Label ++ "]".

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

get_function_graph(FuncName, Settings) ->
    FunAst = share:get_fun_ast(FuncName),
    case FunAst of
        not_found -> no_graph;
        _ -> lv:create_localview(FuncName, Settings, false)
    end.

%%% Evaluate Pattern Matching's list of clauses: evaluate every branch alone, then
%%% link every last vertex's branch to a shared vertex with epsilon transaction
pattern_matching(PMList, Label, Data) ->
    VLast = Data#localview.last_vertex,
    G = Data#localview.graph,
    VLastList = explore_pm(PMList, Label, Data),
    VRet = share:add_vertex(G),
    add_edges_recursive(G, VLastList, VRet, 'ɛ', VLast),
    Data#localview{graph = G, last_vertex = VRet}.

%%% Explore every pm's branch and returns the list of last added vertex
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
                        share:warning("Should be clause but it's", C, {AddedVertexList, Couter})
                end
            end,
            {[], 0},
            PMList
        )
    ).

add_vertex_edge(Label, Data) ->
    G = Data#localview.graph,
    LastV = Data#localview.last_vertex,
    V = share:add_vertex(G),
    digraph:add_edge(G, LastV, V, Label),
    Data#localview{last_vertex = V}.

%%% Convert the guard (ast format) to a string
guards_to_string(GlobalToVal) ->
    case GlobalToVal of
        %%% Add guards if there's a guard
        %%% TODO: add more infos
        {op, _, _, _} -> " (guards)";
        _ -> ""
    end.

%%% Link each vertex of a vertex's list to a given vertex, with a defined label
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
