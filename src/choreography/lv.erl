-module(lv).
-include("../share/common_data.hrl").

%%% API
-export([generate/1]).

%%%===================================================================
%%% API
%%%===================================================================

generate(Settings) ->
    [{_, ActorList}] = ets:lookup(?DBMANAGER, ?ACTORLIST),
    lists:foreach(
        fun(Actor) -> create_save_localview(Actor#actor.name, Settings) end,
        ActorList
    ).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

create_save_localview(ActorName, Settings) ->
    ActorAst = common_fun:get_fun_ast(ActorName),
    case ActorAst of
        not_found ->
            io:fwrite("Error: Actor ~p's AST not found~n", [ActorName]);
        _ ->
            % io:fwrite("[LV] Creating a localview for ~p~n", [ActorName]),
            LocalViewData = check_and_get_lv(ActorName, ActorAst, [], Settings),
            G = LocalViewData#wip_lv.graph,
            set_final(G),
            MinGraph = fsa:minimize(G),
            ets:insert(?LOCALVIEW, {ActorName, LocalViewData#wip_lv{graph = MinGraph}}),
            OutputDir = Settings#setting.output_dir,
            common_fun:save_graph_to_file(MinGraph, OutputDir, atol(ActorName), local)
    end.

create_localview(FunName, LocalVars) ->
    ActorAst = common_fun:get_fun_ast(FunName),
    case ActorAst of
        not_found -> io:fwrite("Error: Function ~p's AST not found~n", [FunName]);
        _ -> check_and_get_lv(FunName, ActorAst, LocalVars, #setting{})
    end.

check_and_get_lv(ActorName, Ast, LovalVars, Settings) ->
    LV = common_fun:get_graph(ActorName),
    case LV of
        not_found ->
            BaseData = #wip_lv{
                fun_name = ActorName, fun_ast = Ast, settings = Settings, input_vars = LovalVars
            },
            common_fun:add_vertex(BaseData#wip_lv.graph),
            build_localview(BaseData);
        L ->
            L
    end.

build_localview(Data) ->
    eval_pm_clause(Data#wip_lv.fun_ast, [], Data).

% add_args_to_graph(Gr, Vars, Guard, VStart, SetPm) ->
%     case SetPm of
%         true ->
%             VN = common_fun:add_vertex(Gr),
%             EdLabel = format_label_pm_edge(SetPm, Vars, Guard, "arg "),
%             digraph:add_edge(Gr, VStart, VN, EdLabel),
%             VN;
%         false ->
%             VStart
%     end.

eval_codeline(CodeLine, Data) ->
    if
        is_tuple(CodeLine) ->
            Line = element(2, CodeLine),
            % io:fwrite("Evaluating ~p on line ~p~n", [element(1, CodeLine), Line]),
            ets:insert(?CLINE, {line, Line});
        true ->
            done
    end,
    case CodeLine of
        {clause, _, Vars, _Guard, Content} -> eval_pm_clause(Content, Vars, Data);
        {match, _, RightContent, LeftContent} -> eval_match(RightContent, LeftContent, Data);
        {call, _, Function, ArgList} -> eval_call(Function, ArgList, Data);
        {'case', _, Content, PMList} -> eval_case(Content, PMList, Data);
        {'if', _, PMList} -> eval_pm(PMList, get_base_label(false, "if "), Data);
        {'receive', _, PMList} -> eval_pm(PMList, "receive ", Data);
        {op, _, Op, LeftContent, RightContent} -> eval_op(Op, LeftContent, RightContent, Data);
        {'fun', N, Ast} -> eval_anon_fun(Ast, N, Data);
        {integer, _, Val} -> eval_simple_type(integer, Val, Data);
        {float, _, Val} -> eval_simple_type(float, Val, Data);
        {string, _, Val} -> eval_simple_type(string, Val, Data);
        {nil, _} -> eval_simple_type(nil, [], Data);
        {atom, _, Val} -> eval_atom(Val, Data);
        {cons, _, HeadList, TailList} -> eval_list(HeadList, TailList, Data);
        {map, _, Val} -> eval_map(Val, Data);
        {tuple, _, Val} -> eval_tuple(Val, Data);
        {var, _, VarName} -> eval_variable(VarName, Data);
        [] -> eval_simple_type(list, [], Data);
        [H | T] -> eval_list(H, T, Data);
        _ -> warning("couldn't parse code line", CodeLine, Data)
    end.

warning(String, Content, Data) ->
    [{_, Line}] = ets:lookup(?CLINE, line),
    io:fwrite("[LV] WARNING on line ~p: " ++ String ++ " ~p~n", [Line, Content]),
    Data.

eval_pm_clause(Code, Vars, Data) ->
    LocalV = Data#wip_lv.local_vars,
    FunName = ltoa(Data#wip_lv.fun_name),
    Exist = ets:lookup(?ARGUMENTS, FunName),
    % io:fwrite("Per ~ps salvo ~p esiste ~p~n", [FunName, Vars, Exist]),
    case Exist of
        [] -> ets:insert(?ARGUMENTS, {FunName, Vars});
        [{_, []}] -> ets:insert(?ARGUMENTS, {FunName, Vars});
        _ -> done
    end,
    ND =
        case Vars =:= [] of
            true ->
                Data;
            false ->
                Input = Data#wip_lv.input_vars,
                {LL, _} = lists:foldl(
                    fun({var, _, Name}, {A, In}) ->
                        case In of
                            [] -> {A ++ [#variable{name = Name}], []};
                            [H | T] -> {A ++ [H#variable{name = Name}], T}
                        end
                    end,
                    {[], Input},
                    Vars
                ),
                Data#wip_lv{local_vars = LocalV ++ LL}
        end,
    lists:foldl(
        fun(Line, AccData) -> eval_codeline(Line, AccData) end,
        ND,
        Code
    ).

eval_match(RightContent, LeftContent, Data) ->
    case RightContent of
        {var, _, VarName} -> eval_match_with_var(VarName, LeftContent, Data);
        {tuple, _, VarList} -> eval_match_with_tuple(VarList, LeftContent, Data);
        {cons, _, List} -> eval_match_with_list(List, Data);
        R -> warning("[MATCH] couldn't understand line", R, Data)
    end.

eval_match_with_var(VarName, LeftContent, Data) ->
    NewData = eval_codeline(LeftContent, Data),
    Var = NewData#wip_lv.ret_var,
    L = NewData#wip_lv.local_vars,
    NewVarEntry = Var#variable{name = VarName},
    NewData#wip_lv{ret_var = NewVarEntry, local_vars = L ++ [NewVarEntry]}.

eval_match_with_tuple(VarList, LeftContent, Data) ->
    NewData = eval_codeline(LeftContent, Data),
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
            warning("right content is a tuple but left content is", Var, ND)
    end.

eval_match_with_list(List, Data) -> warning("[MATCH] working in progress", List, Data).

eval_call(Function, ArgList, Data) ->
    case Function of
        {atom, _, Name} -> eval_call_by_atom(Name, ArgList, Data);
        {var, _, VarName} -> eval_call_by_var(VarName, ArgList, Data);
        {remote, _, Package, FunName} -> eval_call_by_package(Package, FunName, ArgList, Data);
        F -> warning("couldn't call function pattern", F, Data)
    end.

eval_call_by_atom(Name, ArgList, Data) ->
    FunName = Data#wip_lv.fun_name,
    case Name of
        FunName -> eval_recursive(ArgList, Data);
        spawn -> eval_spawn(ArgList, Data);
        spawn_monitor -> eval_spawn_monitor(ArgList, Data);
        self -> eval_self(Data);
        register -> eval_register(ArgList, Data);
        _ -> eval_generic_call(Name, ArgList, Data)
    end.

%%% Dubbio: In questo caso bisogna ritornare i Data così come sono o bisogna impostare il last_vertex all'1?
%%% TODO: Cosa fare con la lista degli argument?
eval_recursive(_ArgList, Data) ->
    % io:fwrite("[RECURSIVE] from vertex ~p~n", [Data#wip_lv.last_vertex]),
    digraph:add_edge(Data#wip_lv.graph, Data#wip_lv.last_vertex, 1, 'ɛ'),
    Data#wip_lv{last_vertex = ?UNDEFINED}.

eval_spawn(ArgList, Data) ->
    case ArgList of
        [Content] -> eval_spawn_one(Content, Data);
        %%% TODO: check and implement package
        [_Package, {atom, _, Name}, SpArgList] -> eval_spawn_three(Name, SpArgList, Data);
        %%% TODO: spawn with 2/4 argument
        _ -> warning("couldn't call function pattern", ArgList, Data)
    end.

eval_spawn_one(Content, Data) ->
    G = Data#wip_lv.graph,
    NewData = eval_codeline(Content, Data),
    VLast = NewData#wip_lv.last_vertex,
    VarFound = NewData#wip_lv.ret_var,
    Id = VarFound#variable.value,
    create_save_localview(ltoa(Id), Data#wip_lv.settings),
    C = inc_spawn_counter(Id),
    S = Id ++ "_" ++ integer_to_list(C),
    VNew = common_fun:add_vertex(G),
    digraph:add_edge(G, VLast, VNew, "spawn " ++ S),
    RetVar = #variable{type = pid, value = S},
    Data#wip_lv{ret_var = RetVar, last_vertex = VNew}.

eval_spawn_three(Name, ArgList, Data) ->
    G = Data#wip_lv.graph,
    VLast = Data#wip_lv.last_vertex,
    C = inc_spawn_counter(Name),
    S = atol(Name) ++ "_" ++ integer_to_list(C),
    VNew = common_fun:add_vertex(G),
    Label = "spawn " ++ S,
    digraph:add_edge(G, VLast, VNew, Label),
    NewData = eval_codeline(ArgList, Data),
    EM = NewData#wip_lv.edge_map,
    ND = NewData#wip_lv{edge_map = maps:put(Label, NewData#wip_lv.ret_var, EM)},
    RetVar = #variable{type = pid, value = S},
    ND#wip_lv{ret_var = RetVar, last_vertex = VNew}.

eval_spawn_monitor(ArgList, Data) -> warning("spawn_monitor not yet implemted. Arguments =", ArgList, Data).

inc_spawn_counter(Name) ->
    case ets:lookup(?SPAWNC, ltoa(Name)) of
        [] ->
            ets:insert(?SPAWNC, {Name, 1}),
            0;
        [{_, N}] ->
            ets:insert(?SPAWNC, {Name, N + 1}),
            N
    end.

eval_self(Data) ->
    RetVar = #variable{type = pid_self, value = atol(Data#wip_lv.fun_name)},
    Data#wip_lv{ret_var = RetVar}.

eval_register(ArgList, Data) ->
    [{atom, _, AtomName}, {var, _, VarName}] = ArgList,
    VarFound = find_var(Data, VarName),
    case VarFound of
        not_found ->
            ?UNDEFINED;
        V ->
            case V#variable.type == pid of
                false -> ?UNDEFINED;
                true -> ets:insert(?REGISTERDB, {AtomName, V#variable.value})
            end
    end,
    %%% Approx: ignoring returing value
    Data.

eval_generic_call(Name, ArgList, Data) ->
    % io:fwrite("ARG LIST ~p~n", [ArgList]),
    NewData = eval_codeline(ArgList, Data),
    % io:fwrite("RET VAR ~p~n", [NewData#wip_lv.ret_var#variable.value]),
    case eval_func(Name, NewData#wip_lv.ret_var#variable.value) of
        no_graph ->
            warning("couldn't parse function", Name, Data#wip_lv{ret_var = #variable{}});
        NewD ->
            G = Data#wip_lv.graph,
            LastV = Data#wip_lv.last_vertex,
            NewG = NewD#wip_lv.graph,
            NewRet = NewD#wip_lv.ret_var,
            NewLastV = merge_graph(G, NewG, LastV),
            Data#wip_lv{ret_var = NewRet, last_vertex = NewLastV}
    end.

%%% TODO: eval argument list
eval_call_by_var(VarName, ArgList, Data) ->
    G = Data#wip_lv.graph,
    VLast = Data#wip_lv.last_vertex,
    LocalVarL = Data#wip_lv.local_vars,
    VarFound = find_var(LocalVarL, VarName),
    case VarFound of
        not_found ->
            warning("variable not found in eval_call_by_var with name", VarName, Data);
        _ ->
            Id = VarFound#variable.value,
            ND = eval_codeline(ArgList, Data),
            NewData = create_localview(ltoa(Id), ND#wip_lv.ret_var#variable.value),
            NewG = NewData#wip_lv.graph,
            NewRet = NewData#wip_lv.ret_var,
            NewLastV = merge_graph(G, NewG, VLast),
            Data#wip_lv{ret_var = NewRet, last_vertex = NewLastV}
    end.

eval_call_by_package(Package, FunName, _ArgList, Data) ->
    {atom, _, Pack} = Package,
    {atom, _, Name} = FunName,
    case Pack of
        rand -> eval_rand_package(Name, Data);
        %%% TODO: find the package and the function, create the local view of it, attach it to the current lv
        _ -> warning("package not yet implemented:", Pack, Data)
    end.

eval_rand_package(FunName, Data) ->
    case FunName of
        uniform -> eval_simple_type(integer, ?ANYDATA, Data);
        %%% TODO: expand
        _ -> warning("rand's function not yet implemented:", FunName, Data)
    end.

eval_case(Content, PMList, Data) ->
    BaseL =
        case Content of
            {var, _, VarName} -> get_base_label(false, atol(VarName) ++ " match ");
            _ -> get_base_label(false, "match smt")
        end,
    eval_pm(PMList, BaseL, Data).

eval_op(Op, LeftContent, RightContent, Data) ->
    case Op of
        '!' -> eval_send(LeftContent, RightContent, Data);
        %%% TODO: implement other basic operation
        _ -> warning("operation not yet implemented", Op, Data)
    end.

eval_send(Destination, MessageContent, Data) ->
    G = Data#wip_lv.graph,
    TempData = eval_codeline(Destination, Data),
    ProcName = get_pid(TempData#wip_lv.ret_var),
    NewData = eval_codeline(MessageContent, TempData),
    LastV = NewData#wip_lv.last_vertex,
    DataSent = recordvar_to_string(NewData#wip_lv.ret_var),
    VNew = common_fun:add_vertex(G),
    SLabel = "send " ++ DataSent ++ " to " ++ ProcName,
    digraph:add_edge(G, LastV, VNew, SLabel),
    NewData#wip_lv{last_vertex = VNew}.

get_pid(Var) ->
    case Var#variable.type == pid of
        true -> Var#variable.value;
        false -> atol(Var#variable.name)
    end.

eval_anon_fun(Content, N, Data) ->
    case Content of
        {clauses, A} ->
            Id = "anonfun_line" ++ integer_to_list(N),
            ets:insert(?FUNAST, {ltoa(Id), A}),
            eval_simple_type(function, Id, Data);
        _ ->
            warning("not recognized content in eval_anon_fun", Content, Data)
    end.

eval_simple_type(Type, Val, Data) ->
    VarRet = #variable{type = Type, value = Val},
    Data#wip_lv{ret_var = VarRet}.

eval_atom(Val, Data) ->
    %%% check if is a registerd atom, return the pid
    case ets:lookup(?REGISTERDB, Val) of
        [] -> eval_simple_type(atom, Val, Data);
        [{_, Pid}] -> eval_simple_type(pid, Pid, Data)
    end.

eval_list(HeadList, TailList, Data) ->
    NDH = eval_codeline(HeadList, Data),
    NDT = eval_codeline(TailList, NDH),
    Var = NDH#wip_lv.ret_var,
    VarList = NDT#wip_lv.ret_var,
    NewVal = [Var] ++ VarList#variable.value,
    eval_simple_type(list, NewVal, NDT).

%%% TODO
eval_map(_Val, Data) ->
    Data.

eval_tuple(Val, Data) ->
    {NewVal, NewData} = lists:foldl(
        fun(I, {A, D}) ->
            ND = eval_codeline(I, D),
            {A ++ [ND#wip_lv.ret_var], ND}
        end,
        {[], Data},
        Val
    ),
    eval_simple_type(tuple, NewVal, NewData).

eval_variable(VarName, Data) ->
    Var = find_var(Data#wip_lv.local_vars, VarName),
    RetV =
        case Var of
            not_found -> #variable{name = VarName};
            V -> V
        end,
    Data#wip_lv{ret_var = RetV}.

recordvar_to_string(Var) ->
    case Var#variable.type of
        ?ANYDATA ->
            atol(Var#variable.name);
        Type ->
            SType = atol(Type),
            case Var#variable.value of
                ?ANYDATA ->
                    SType;
                Val ->
                    case SType of
                        "integer" -> integer_to_list(Val);
                        "float" -> io_lib:format("~.2f", [Val]);
                        "string" -> "[" ++ Val ++ "]";
                        "atom" -> atol(Val);
                        "tuple" -> format_tuple(Val, fun recordvar_to_string/1);
                        "pid" -> "pid" ++ "_" ++ atol(Var#variable.value);
                        S -> S
                    end
            end
    end.

format_tuple(VarL, Fun) ->
    L = lists:foldl(fun(I, A) -> A ++ "," ++ Fun(I) end, "", VarL),
    [_ | Label] = L,
    "{" ++ Label ++ "}".

get_base_label(SetPm, Label) ->
    case SetPm of
        true -> Label;
        false -> 'ɛ'
    end.

find_var([], _) ->
    not_found;
find_var([Var | Tail], Name) ->
    Cond = Var#variable.name =:= Name,
    case Cond of
        true -> Var;
        false -> find_var(Tail, Name)
    end;
find_var(Data, Name) ->
    LL = Data#wip_lv.local_vars,
    find_var(LL, Name).

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
        fun(Item, M) -> maps:put(Item, common_fun:add_vertex(MainG), M) end,
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

eval_func(FuncName, LV) ->
    FunAst = common_fun:get_fun_ast(FuncName),
    case FunAst of
        not_found -> no_graph;
        _ -> create_localview(FuncName, LV)
    end.

%%% Evaluate Pattern Matching's list of clauses: evaluate every branch alone, then
%%% link every last vertex's branch to a shared vertex with epsilon transaction
eval_pm(PMList, Label, Data) ->
    VLast = Data#wip_lv.last_vertex,
    G = Data#wip_lv.graph,
    VLastList = explore_pm(PMList, Label, Data),
    VRet = common_fun:add_vertex(G),
    add_edges_recursive(G, VLastList, VRet, 'ɛ', VLast),
    Data#wip_lv{graph = G, last_vertex = VRet}.

%%% Explore every pm's branch and returns the list of last added vertex
explore_pm(PMList, Base, Data) ->
    G = Data#wip_lv.graph,
    VLast = Data#wip_lv.last_vertex,
    lists:foldl(
        fun(CodeLine, AddedVertexList) ->
            case CodeLine of
                {clause, _, Vars, Guard, Content} ->
                    V = common_fun:add_vertex(G),
                    digraph:add_edge(G, VLast, V, 'ɛ'),
                    IsReceive = is_list(string:find(atol(Base), "receive")),
                    %%% if it's a receive pm, then the label must be written
                    EdLabel = format_label_pm_edge(IsReceive, Vars, Guard, atol(Base)),
                    VL =
                        case IsReceive of
                            true ->
                                VNew = common_fun:add_vertex(G),
                                digraph:add_edge(G, V, VNew, EdLabel),
                                VNew;
                            false ->
                                V
                        end,
                    VDataRet = eval_pm_clause(Content, [], Data#wip_lv{last_vertex = VL}),
                    AddedVertexList ++ [VDataRet#wip_lv.last_vertex];
                _ ->
                    AddedVertexList
            end
        end,
        [],
        PMList
    ).

%%% Format the Variables with the guards in a label for the FSA
format_label_pm_edge(SetPm, VarList, GuardList, BaseLabel) when is_list(BaseLabel) ->
    case SetPm of
        true ->
            VarsS = lists:foldl(fun(V, Acc) -> astvar_to_s(V, Acc) ++ ", " end, BaseLabel, VarList),
            VarGuardS = lists:foldl(fun(G, Acc) -> guards_to_s(G, Acc) end, VarsS, GuardList),
            remove_last(remove_last(VarGuardS));
        false ->
            'ɛ'
    end.

%%% Remove the last element froom a list
remove_last(List) when is_list(List) ->
    {Rest, _} = lists:split(length(List) - 1, List),
    Rest.

%%% Convert a data type (ast format) to a string
astvar_to_s(VarToVal) ->
    astvar_to_s(VarToVal, "").
astvar_to_s(VarToVal, BaseL) ->
    BaseL ++
        case VarToVal of
            %% TODO: espandere o fare refactor di sta parte
            {integer, _, Value} -> integer_to_list(Value);
            {var, _, '_'} -> "_";
            {tuple, _, LVar} -> format_tuple(LVar, fun astvar_to_s/1);
            {var, _, Var} -> atol(Var);
            {atom, _, Atom} -> atol(Atom);
            {cons, _, H, T} -> "[" ++ astvar_to_s(H) ++ ", " ++ astvar_to_s(T) ++ "]";
            {nil, _} -> "null";
            _ -> atol(?UNDEFINED)
        end.

%%% Convert the guard (ast format) to a string
guards_to_s(GlobalToVal, BaseL) ->
    case GlobalToVal of
        %%% Add guards if there's a guard
        %%% TODO: add more infos
        {op, _, _, _} -> BaseL ++ " (guards)";
        _ -> BaseL
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

%%% Set vertices as a final state if they do not have out edges
set_final(Graph) ->
    VL = digraph:vertices(Graph),
    lists:foreach(
        fun(Vertex) ->
            OD = digraph:out_degree(Graph, Vertex),
            {_, Label} = digraph:vertex(Graph, Vertex),
            case OD =:= 0 of
                true ->
                    FormattedLabel = ?FINALTAG ++ integer_to_list(Label),
                    digraph:add_vertex(Graph, Vertex, FormattedLabel);
                false ->
                    do_nothing
            end
        end,
        VL
    ).

ltoa(L) when is_list(L) -> list_to_atom(L);
ltoa(L) when is_atom(L) -> L.
atol(A) when is_atom(A) -> atom_to_list(A);
atol(A) when is_list(A) -> A.
