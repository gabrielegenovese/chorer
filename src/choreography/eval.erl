-module(eval).
-include("../share/common_data.hrl").

%%% API
-export([
    function_list/2,
    clause/5,
    match/3,
    case_pm/3,
    if_pm/2,
    receive_pm/2,
    operation/4,
    function_call/3,
    anon_fun/3,
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

function_list(ContList, Data) ->
    lists:foldl(
        fun(FunctionBody, AccData) ->
            case FunctionBody of
                {clause, _, Vars, Guard, Content} ->
                    % io:fwrite("Clause ~p~n", [Vars]),
                    ets:insert(?ARGUMENTS, {Data#wip_lv.fun_name, Vars}),
                    clause(Content, Vars, Guard, AccData#wip_lv{last_vertex = 1}, "arg");
                C ->
                    share:warning("should be a clause but it's", C, ?UNDEFINED)
            end
        end,
        Data,
        ContList
    ).

clause(Code, Vars, Guards, Data, BaseLabel) ->
    % io:fwrite("[CLAUSE] Code ~p~n Vars ~p~n Guards ~p~n", [Code, Vars, Guards]),
    LocalV = Data#wip_lv.local_vars,
    TempData = lv:eval_codeline(Vars, Data),
    % should always be a list
    EvalVarList = TempData#wip_lv.ret_var,
    TempLabel = BaseLabel ++ " " ++ var_to_string(EvalVarList) ++ guards_to_string(Guards),
    FinalLabel = decide_label(BaseLabel, TempLabel, Data),
    NewData = add_vertex_edge(FinalLabel, Data),
    lists:foldl(
        fun(Line, AccData) -> lv:eval_codeline(Line, AccData) end,
        NewData#wip_lv{local_vars = LocalV ++ EvalVarList#variable.value},
        Code
    ).

match(RightContent, LeftContent, Data) ->
    case RightContent of
        {var, _, VarName} -> match_with_var(VarName, LeftContent, Data);
        {tuple, _, VarList} -> match_with_tuple(VarList, LeftContent, Data);
        {cons, _, List} -> match_with_list(List, Data);
        R -> share:warning("[MATCH] couldn't understand line", R, Data)
    end.

case_pm(Content, PMList, Data) ->
    NewData = lv:eval_codeline(Content, Data),
    %% TODO: save the returning variable?
    pattern_matching(PMList, "match", NewData).

if_pm(PMList, Data) ->
    pattern_matching(PMList, "if", Data).

receive_pm(PMList, Data) ->
    pattern_matching(PMList, "receive", Data).

operation(Symbol, LeftContent, RightContent, Data) ->
    case Symbol of
        '!' -> send(LeftContent, RightContent, Data);
        %%% TODO: implement other basic operation
        _ -> share:warning("operation not yet implemented", Symbol, Data)
    end.

function_call(Function, ArgList, Data) ->
    case Function of
        {atom, _, Name} -> call_by_atom(Name, ArgList, Data);
        {var, _, VarName} -> call_by_var(VarName, ArgList, Data);
        {remote, _, Package, FunName} -> call_by_package(Package, FunName, ArgList, Data);
        F -> share:warning("couldn't call function pattern", F, Data)
    end.

anon_fun(Content, Line, Data) ->
    case Content of
        {clauses, A} ->
            Id = "anonfun_" ++ integer_to_list(Line),
            ets:insert(?FUNAST, {Id, {function, Line, A}}),
            simple_type(function, Id, Data);
        _ ->
            share:warning("not recognized content in anon_fun", Content, Data)
    end.

simple_type(Type, Val, Data) ->
    VarRet = #variable{type = Type, value = Val},
    Data#wip_lv{ret_var = VarRet}.

atom(Val, Data) ->
    %%% check if is a registerd atom, return the pid
    case ets:lookup(?REGISTERDB, Val) of
        [] -> simple_type(atom, Val, Data);
        [{_, Pid}] -> simple_type(pid, Pid, Data)
    end.

list(HeadList, TailList, Data) ->
    NDH = lv:eval_codeline(HeadList, Data),
    NDT = lv:eval_codeline(TailList, NDH),
    Var = NDH#wip_lv.ret_var,
    VarList = NDT#wip_lv.ret_var,
    NewVal = [Var] ++ VarList#variable.value,
    simple_type(list, NewVal, NDT).

map(Val, Data) ->
    share:warning("TODO map evaluation", Val, Data).

tuple(Val, Data) ->
    {NewVal, NewData} = lists:foldl(
        fun(I, {A, D}) ->
            ND = lv:eval_codeline(I, D),
            {A ++ [ND#wip_lv.ret_var], ND}
        end,
        {[], Data},
        Val
    ),
    simple_type(tuple, NewVal, NewData).

variable(VarName, Data) ->
    Var = share:find_var(Data#wip_lv.local_vars, VarName),
    RetV =
        case Var of
            not_found -> #variable{name = VarName};
            V -> V
        end,
    Data#wip_lv{ret_var = RetV}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

decide_label(Base, Label, Data) ->
    Settings = Data#wip_lv.settings,
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
    Var = NewData#wip_lv.ret_var,
    L = NewData#wip_lv.local_vars,
    NewVarEntry = Var#variable{name = VarName},
    NewData#wip_lv{ret_var = NewVarEntry, local_vars = L ++ [NewVarEntry]}.

match_with_tuple(VarList, LeftContent, Data) ->
    NewData = lv:eval_codeline(LeftContent, Data),
    Var = NewData#wip_lv.ret_var,
    % not sure to use newdata
    L = NewData#wip_lv.local_vars,
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
            NewData#wip_lv{
                ret_var = #variable{type = tuple, value = TupleListWithNames},
                local_vars = L ++ TupleListWithNames
            };
        _ ->
            RetVar = lists:foldl(fun(I, A) -> A ++ [#variable{name = I}] end, [], VarList),
            ND = NewData#wip_lv{
                ret_var = #variable{type = tuple, value = RetVar},
                local_vars = L ++ RetVar
            },
            share:warning("right content is a tuple but left content is", Var, ND)
    end.

match_with_list(List, Data) ->
    share:warning("[MATCH] TODO match with list", List, Data).

call_by_atom(Name, ArgList, Data) ->
    FunName = Data#wip_lv.fun_name,
    RealName = share:ltoa(share:remove_last(FunName)),
    case Name of
        RealName -> recursive(ArgList, Data);
        spawn -> spawn_call(ArgList, Data);
        spawn_monitor -> spawn_monitor_call(ArgList, Data);
        self -> self_call(Data);
        register -> register_call(ArgList, Data);
        _ -> generic_call(Name, ArgList, Data)
    end.

%%% TODO: What to do with argument lists? Put them in the #wip_lv.edge_map?
recursive(_ArgList, Data) ->
    % io:fwrite("[RECURSIVE] from vertex ~p~n", [Data#wip_lv.last_vertex]),
    digraph:add_edge(Data#wip_lv.graph, Data#wip_lv.last_vertex, 1, 'ɛ'),
    Data#wip_lv{last_vertex = ?UNDEFINED}.

spawn_call(ArgList, Data) ->
    case ArgList of
        [Content] -> spawn_one(Content, Data);
        %%% TODO: check and implement package
        [_Package, {atom, _, Name}, SpArgList] -> spawn_three(Name, SpArgList, Data);
        %%% TODO: spawn with 2/4 argument
        _ -> share:warning("couldn't call function pattern", ArgList, Data)
    end.

spawn_one(Content, Data) ->
    NewData = lv:eval_codeline(Content, Data),
    VarFound = NewData#wip_lv.ret_var,
    Id = VarFound#variable.value,
    lv:create_localview(Id, Data#wip_lv.settings, true),
    C = share:inc_spawn_counter(Id),
    S = Id ++ ?SEPARATOR ++ integer_to_list(C),
    RetData = add_vertex_edge("spawn " ++ S, NewData),
    RetVar = #variable{type = pid, value = S},
    RetData#wip_lv{ret_var = RetVar}.

spawn_three(Name, ArgList, Data) ->
    NewData = lv:eval_codeline(ArgList, Data),
    NewDataRetVar = NewData#wip_lv.ret_var,
    {Label, ProcId} = format_spawn_label(Name, NewDataRetVar),
    EM = maps:put(Label, NewDataRetVar, NewData#wip_lv.edge_map),
    % io:fwrite("label ~p new edge map ~p map ~p~n", [Label, NewData#wip_lv.fun_name, EM]),
    ND = NewData#wip_lv{edge_map = EM},
    RetData = add_vertex_edge(Label, ND),
    RetData#wip_lv{ret_var = #variable{type = pid, value = ProcId}}.

format_spawn_label(Name, NewDataRetVar) ->
    C = share:inc_spawn_counter(Name),
    Arity = integer_to_list(length(NewDataRetVar#variable.value)),
    ProcId = share:atol(Name) ++ Arity ++ ?SEPARATOR ++ integer_to_list(C),
    {"spawn " ++ ProcId, ProcId}.

spawn_monitor_call(ArgList, Data) ->
    share:warning("spawn_monitor not yet implemted. Arguments =", ArgList, Data).

self_call(Data) ->
    RetVar = #variable{type = pid, value = "pid_self"},
    Data#wip_lv{ret_var = RetVar}.

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
    % io:fwrite("RET VAR ~p~n", [NewData#wip_lv.ret_var#variable.value]),
    NameString = share:merge_fun_ar(Name, length(ArgList)),
    case get_function_graph(NameString, Data#wip_lv.settings) of
        no_graph ->
            share:warning("couldn't parse function", Name, Data#wip_lv{ret_var = #variable{}});
        NewD ->
            G = Data#wip_lv.graph,
            LastV = Data#wip_lv.last_vertex,
            NewG = NewD#wip_lv.graph,
            NewRet = NewD#wip_lv.ret_var,
            EM = NewD#wip_lv.edge_map,
            NewLastV = merge_graph(G, NewG, LastV),
            % io:fwrite("LastV ~p VarRet ~p~n", [LastV, NewRet]),
            % io:fwrite("OldEM ~p NewEM ~p~n", [Data#wip_lv.edge_map, EM]),
            Data#wip_lv{
                ret_var = NewRet,
                last_vertex = NewLastV,
                edge_map = maps:merge(Data#wip_lv.edge_map, EM)
            }
    end.

%%% TODO: eval argument list
call_by_var(VarName, ArgList, Data) ->
    G = Data#wip_lv.graph,
    VLast = Data#wip_lv.last_vertex,
    LocalVarL = Data#wip_lv.local_vars,
    VarFound = share:find_var(LocalVarL, VarName),
    case VarFound of
        not_found ->
            share:warning("variable not found in call_by_var with name", VarName, Data);
        _ ->
            Id = VarFound#variable.value,
            ND = lv:eval_codeline(ArgList, Data),
            NewData = lv:create_localview(Id, ND#wip_lv.settings, false),
            NewG = NewData#wip_lv.graph,
            NewRet = NewData#wip_lv.ret_var,
            NewLastV = merge_graph(G, NewG, VLast),
            Data#wip_lv{ret_var = NewRet, last_vertex = NewLastV}
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
        %%% TODO: expand
        _ -> share:warning("rand's function not yet implemented:", FunName, Data)
    end.

send(Destination, MessageContent, Data) ->
    TempData = lv:eval_codeline(Destination, Data),
    VarProcName = TempData#wip_lv.ret_var,
    ProcName = get_pid(VarProcName),
    % io:fwrite("Ret Var ~p found ~p~n", [VarProcName, ProcName]),
    NewData = lv:eval_codeline(MessageContent, TempData),
    VarDataSent = NewData#wip_lv.ret_var,
    DataSent = var_to_string(VarDataSent),
    SLabel = "send " ++ share:atol(DataSent) ++ " to " ++ share:atol(ProcName),
    EM = NewData#wip_lv.edge_map,
    add_vertex_edge(SLabel, NewData#wip_lv{
        edge_map = maps:put(SLabel, {VarProcName, VarDataSent}, EM)
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
    VLast = Data#wip_lv.last_vertex,
    G = Data#wip_lv.graph,
    VLastList = explore_pm(PMList, Label, Data),
    VRet = share:add_vertex(G),
    add_edges_recursive(G, VLastList, VRet, 'ɛ', VLast),
    Data#wip_lv{graph = G, last_vertex = VRet}.

%%% Explore every pm's branch and returns the list of last added vertex
explore_pm(PMList, Base, Data) ->
    lists:foldl(
        fun(CodeLine, AddedVertexList) ->
            case CodeLine of
                {clause, _, Vars, Guard, Content} ->
                    VDataRet = clause(Content, Vars, Guard, Data, Base),
                    AddedVertexList ++ [VDataRet#wip_lv.last_vertex];
                C ->
                    share:warning("Should be clause but it's", C, AddedVertexList)
            end
        end,
        [],
        PMList
    ).

add_vertex_edge(Label, Data) ->
    G = Data#wip_lv.graph,
    LastV = Data#wip_lv.last_vertex,
    V = share:add_vertex(G),
    digraph:add_edge(G, LastV, V, Label),
    Data#wip_lv{last_vertex = V}.

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
