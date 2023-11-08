-module(local_view).
-include("../share/common_data.hrl").

%%% API
-export([generate/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%% Generate a local view for each actor
generate(OutputDir, Options) ->
    ActorList = db_manager:get_actors(),
    %%% For each actor, create and save the local view
    lists:foreach(
        fun(Actor) -> create_localview(Actor, OutputDir, Options) end,
        ActorList
    ).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%% Create a local view for an actor
create_localview(ActorName, OutputDir, Options) ->
    ActorAst = db_manager:get_fun_ast(ActorName),
    case ActorAst of
        no_ast_found ->
            io:fwrite("Error: Actor ~p's AST not found~n", [ActorName]);
        _ ->
            TempG = db_manager:get_fun_graph(ActorName),
            case TempG of
                no_graph_found ->
                    {SetFinalS, SetAdditionalInfo} = Options,
                    G = get_localview(ActorName, ActorAst, [], SetFinalS, SetAdditionalInfo);
                G ->
                    G
            end,
            MinGraph = fsa:minimize(G),
            db_manager:send_fun_graph(ActorName, MinGraph),
            %%% Send the graph to the dbmanager
            common_fun:save_graph_to_file(MinGraph, OutputDir, atol(ActorName), local)
    end.

%%% Get the local view of a function
get_localview(FunName, Code, LocalVars, _SetFinal, SetPm) when is_list(Code) ->
    Gr = digraph:new(),
    VStart = common_fun:add_vertex(Gr),
    lists:foreach(
        fun(Line) ->
            case Line of
                {clause, _, Vars, Guard, Content} ->
                    %%% Show the pattern matching options to view the complete local view
                    VN = add_args_to_graph(Gr, Vars, Guard, VStart, SetPm),
                    _VFinal = eval_pm_clause(Content, FunName, Gr, VN, LocalVars, false);
                % TODO: capire perché questa linea rompe tutto.
                % set_as_final(SetFinal, Gr, VFinal);
                _ ->
                    ?UNDEFINED
            end
        end,
        Code
    ),
    Gr.

%%% Add "Argument" Edges to the local view.
add_args_to_graph(Gr, Vars, Guard, VStart, SetPm) ->
    case SetPm of
        true ->
            VN = common_fun:add_vertex(Gr),
            EdLabel = format_label_pm_edge(SetPm, Vars, Guard, "arg "),
            digraph:add_edge(Gr, VStart, VN, EdLabel),
            VN;
        false ->
            VStart
    end.

%%% Evaluate each codeline of a function and returns the last vertex added to the graph
eval_pm_clause(Code, FunName, Gr, VStart, LocalVarS, SetPm) ->
    {_, LastVertexAdded, LocalVars} = lists:foldl(
        fun(Line, AccData) -> eval_codeline(Line, FunName, Gr, AccData, SetPm) end,
        {#variable{}, VStart, LocalVarS},
        Code
    ),
    db_manager:add_fun_local_vars(FunName, LocalVars),
    LastVertexAdded.

%%% Evaluate a single line of code and returns the last vertex added to the graph and the list of local variables
eval_codeline(CodeLine, FunName, G, AccData, SetPm) ->
    {LastRetVar, VLast, LocalVarL} = AccData,
    case CodeLine of
        %%% In a match code line (Var = ...), evaluate the left
        %%% content because it could be a receive, case, if, etc...
        {match, _, {var, _, VarName}, LeftContent} ->
            {Var, V, NewL} = eval_codeline(LeftContent, FunName, G, AccData, SetPm),
            NewVarEntry = Var#variable{name = VarName},
            {Var, V, NewL ++ [NewVarEntry]};
        %%% If there's a recursive call, just add an epsilon edge to the first state
        {call, _, {atom, _, FunName}, _ArgList} ->
            digraph:add_edge(G, VLast, 1, 'ɛ'),
            {#variable{}, 1, LocalVarL};
        %%% Evaluate the spawn() function
        {call, _, {atom, _, spawn}, [_, {atom, _, Name}, ArgList]} ->
            C = db_manager:inc_spawn_counter(Name),
            S = atol(Name) ++ "_" ++ integer_to_list(C),
            VNew = common_fun:add_vertex(G),
            digraph:add_edge(G, VLast, VNew, "spawn " ++ S),
            {VarArgL, _, _} = eval_codeline(ArgList, FunName, G, AccData, SetPm),
            db_manager:add_fun_arg(S, VarArgL#variable.value),
            {#variable{type = ltoa("pid_" ++ S)}, VNew, LocalVarL};
        %%% Evaluate the self() function
        {call, _, {atom, _, self}, _} ->
            {#variable{type = ltoa("pid_self")}, VLast, LocalVarL};
        %%% Evaluate the register() function
        {call, _, {atom, _, register}, [{atom, _, AtomV}, {var, _, VarV}]} ->
            manage_register(LocalVarL, AtomV, VarV),
            io:fwrite("[LOCAL] AtomV ~p VarV ~p~n", [AtomV, VarV]),
            AccData;
        %%% Evaluate a generic function
        %%% Attention: don't change the position of this pattern matching branch
        %%% TODO: implement argument list evaluation feature
        {call, _, {atom, _, Name}, ArgList} ->
            {V, _, NewL} = eval_codeline(ArgList, FunName, G, AccData, SetPm),
            io:fwrite("[LOCAL] V ~p NewL ~p Name ~p~n", [V, NewL, FunName]),
            NewG =
                case db_manager:get_fun_graph(Name) of
                    no_graph_found -> eval_func(Name, [], SetPm);
                    G -> G
                end,
            db_manager:send_fun_graph(Name, NewG),
            case NewG of
                %%% If the function called is a built-in or a module function,
                %%% then we don't have the Ast. Just return the last added vertex
                no_graph -> AccData;
                %%% If there's no error, then we have a graph, then we need to
                %%% add the graph to the main graph
                _ -> {LastRetVar, merge_graph(G, NewG, VLast), LocalVarL}
            end;
        %%% Evaluate a rand:uniform() function
        {call, _, {remote, _, {atom, _, rand}, {atom, _, uniform}}, _ArgList} ->
            {#variable{type = integer}, VLast, LocalVarL};
        %%% Evaluate an extern function
        {call, _, {remote, _, {atom, _, _Package}, {atom, _, _Name}}, _ArgList} ->
            %%% TODO: find the package and the function, create the local view of it
            AccData;
        %%% Evaluate Case and If
        {'case', _, {var, _, Var}, PMList} ->
            BaseLabel = get_base_label(SetPm, atol(Var) ++ " match "),
            {
                #variable{},
                eval_pm(PMList, G, VLast, LocalVarL, FunName, BaseLabel, SetPm),
                LocalVarL
            };
        {'case', _, _, PMList} ->
            BaseLabel = get_base_label(SetPm, "match smt"),
            {
                #variable{},
                eval_pm(PMList, G, VLast, LocalVarL, FunName, BaseLabel, SetPm),
                LocalVarL
            };
        {'if', _, PMList} ->
            BaseLabel = get_base_label(SetPm, "if "),
            {
                #variable{},
                eval_pm(PMList, G, VLast, LocalVarL, FunName, BaseLabel, SetPm),
                LocalVarL
            };
        %%% Evaluate Receive
        {'receive', _, PMList} ->
            {
                #variable{},
                eval_pm(PMList, G, VLast, LocalVarL, FunName, "receive ", SetPm),
                LocalVarL
            };
        %%% Evaluate Send
        {op, _, '!', {_, _, VarName}, DataSentAst} ->
            {Var, _, NewL} = eval_codeline(DataSentAst, FunName, G, AccData, SetPm),
            DataSent = recordvar_to_string(Var),
            VNew = common_fun:add_vertex(G),
            IsVar = common_fun:is_erlvar(VarName),
            ProcSentTo =
                case IsVar of
                    true ->
                        ArgL =
                            case db_manager:get_fun_args(FunName) of
                                no_fun_args_found -> [];
                                L -> L
                            end,
                        find_proc_in_varl(VarName, LocalVarL ++ ArgL);
                    false ->
                        RegList = db_manager:get_reg_entry(),
                        find_in_register(RegList, ltoa(VarName))
                end,
            SLabel =
                "send " ++ DataSent ++ " to " ++
                    case ProcSentTo of
                        nomatch -> atol(VarName);
                        self -> atol(FunName);
                        "self" -> atol(FunName);
                        P -> atol(P)
                    end,
            digraph:add_edge(G, VLast, VNew, SLabel),
            {#variable{type = atom, value = DataSent}, VNew, NewL};
        %%% Evaluate Types
        {cons, _, HeadList, TailList} ->
            {Var, _, _} = eval_codeline(HeadList, FunName, G, AccData, SetPm),
            {VarList, _, _} = eval_codeline(TailList, FunName, G, AccData, SetPm),
            NewVal = [Var] ++ VarList#variable.value,
            {VarList#variable{value = NewVal}, VLast, LocalVarL};
        {integer, _, Val} ->
            {#variable{type = integer, value = Val}, VLast, LocalVarL};
        {float, _, Val} ->
            {#variable{type = float, value = Val}, VLast, LocalVarL};
        {string, _, Val} ->
            {#variable{type = string, value = Val}, VLast, LocalVarL};
        {atom, _, Val} ->
            {#variable{type = atom, value = Val}, VLast, LocalVarL};
        {tuple, _, TupleVal} ->
            L = lists:foldl(
                fun(I, A) ->
                    {V, _, _} = eval_codeline(I, FunName, G, AccData, SetPm),
                    A ++ [V]
                end,
                [],
                TupleVal
            ),
            {#variable{type = tuple, value = L}, VLast, LocalVarL};
        {nil, _} ->
            {#variable{type = list, value = []}, VLast, LocalVarL};
        {var, _, VarName} ->
            %%% TODO:find var in argument vars
            % ArgL = db_manager:get_fun_args(FunName),
            VarF = find_var(LocalVarL, VarName),
            case VarF of
                not_found -> {#variable{name = VarName}, VLast, LocalVarL};
                _ -> {VarF, VLast, LocalVarL}
            end;
        _ ->
            io:fwrite("WARNING: couldn't parse code line ~p~n", [CodeLine]),
            AccData
    end.

%%% Find a process in a list of variables
find_proc_in_varl(_, []) ->
    nomatch;
find_proc_in_varl(Name, [H | T]) ->
    VarN = H#variable.name,
    S = atol(H#variable.type),
    case Name =:= VarN of
        true -> string:prefix(S, "pid_");
        false -> find_proc_in_varl(Name, T)
    end.

%%% Convert a variable (record) to a string
recordvar_to_string(Var) ->
    io:fwrite("[Var] ~p~n", [Var]),
    case Var#variable.type of
        ?UNDEFINED ->
            atol(Var#variable.name);
        Type ->
            SType = atol(Type),
            case Var#variable.value of
                ?UNDEFINED ->
                    SType;
                Val ->
                    case SType of
                        "integer" -> integer_to_list(Val);
                        "float" -> io_lib:format("~.2f", [Val]);
                        "string" -> "[" ++ Val ++ "]";
                        "atom" -> atol(Val);
                        "tuple" -> format_tuple(Val, fun recordvar_to_string/1);
                        "pid" -> "pid";
                        _ -> SType
                    end
            end
    end.

%%% Convert a tuple to a string
format_tuple(VarL, Fun) ->
    L = lists:foldl(fun(I, A) -> A ++ "," ++ Fun(I) end, "", VarL),
    [_ | Label] = L,
    "{" ++ Label ++ "}".

get_base_label(SetPm, Label) ->
    case SetPm of
        true -> Label;
        false -> 'ɛ'
    end.

%%% Send the info for the register() function to the db manager
manage_register(LocalVarL, AtomName, VarName) ->
    VarFound = find_var(LocalVarL, VarName),
    case VarFound of
        not_found ->
            ?UNDEFINED;
        V ->
            IsPid = is_type_pid(V#variable.type),
            Pid = get_pid_from_type(V#variable.type),
            case IsPid of
                false -> ?UNDEFINED;
                true -> db_manager:add_reg_entry({AtomName, Pid})
            end
    end.

%%% Find variable by Name in a list
find_var([], _) ->
    not_found;
find_var([Var | Tail], Name) ->
    Cond = Var#variable.name =:= Name,
    case Cond of
        true -> Var;
        false -> find_var(Tail, Name)
    end.

%%% If the input is a string with "pid" in it returns true, otherwise false.
is_type_pid(Type) ->
    SL = atol(Type),
    IsPid = string:find(SL, "pid"),
    is_list(IsPid).

%%% Remove "pid_" from a string
get_pid_from_type(Type) ->
    SL = atol(Type),
    string:prefix(SL, "pid_").

%%% Find a registered atom in a list
find_in_register([], _) ->
    nomatch;
find_in_register([{RegAtom, RegPid} | T], Name) ->
    case RegAtom =:= Name of
        true -> RegPid;
        false -> find_in_register(T, Name)
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

%%% Returns an FSA graph of the function if the ast exist, otherwise return error.
eval_func(FuncName, LocalVar, SetPm) ->
    FunAst = db_manager:get_fun_ast(FuncName),
    case FunAst of
        no_ast_found -> no_graph;
        %%% get the graph but don't set the final state
        _ -> get_localview(FuncName, FunAst, LocalVar, false, SetPm)
    end.

%%% Evaluate Pattern Matching's list of clauses: evaluate every branch alone, then
%%% link every last vertex's branch to a shared vertex with epsilon transaction
eval_pm(PMList, G, VLast, LocalVarL, FunName, Label, SetPm) ->
    VLastList = explore_pm(PMList, G, VLast, LocalVarL, FunName, Label, SetPm),
    VRet = common_fun:add_vertex(G),
    add_edges_recursive(G, VLastList, VRet, 'ɛ', VLast),
    VRet.

%%% Explore every pm's branch and returns the list of last added vertex
explore_pm(PMList, G, VLast, LocalVarL, FunName, Base, SetPm) ->
    lists:foldl(
        fun(CodeLine, AddedVertexList) ->
            case CodeLine of
                {clause, _, Vars, Guard, Content} ->
                    IsReceive = is_list(string:find(atol(Base), "receive")),
                    %%% if it's a receive pm, then the label must be written
                    EdLabel = format_label_pm_edge(IsReceive or SetPm, Vars, Guard, atol(Base)),
                    VL =
                        case IsReceive or SetPm of
                            true ->
                                VNew = common_fun:add_vertex(G),
                                digraph:add_edge(G, VLast, VNew, EdLabel),
                                VNew;
                            false ->
                                VLast
                        end,
                    VRet = eval_pm_clause(Content, FunName, G, VL, LocalVarL, SetPm),
                    AddedVertexList ++ [VRet];
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
    %%% Link every V in the VertexList to the VertexToLink, with a specified label
    [
        digraph:add_edge(G, V, VertexToLink, Label)
     || V <- VertexList,
        %%% exclude the start vertex
        V =/= 1,
        V =/= Except
    ].

%%% Set a vertex as a final state
set_as_final(ShouldBeSet, Graph, Vertex) ->
    {_, LastLabel} = digraph:vertex(Graph, Vertex),
    if
        %%% we can add *and (LastLabel =/= 1) ->* to the guard bacause there could be problems with loops
        %%% Future TODO: in eval_codeline potrei tornare il vertice e un booleano che rappresenti se la
        %%% funzione sia un loop o meno e settare lo stato finale di conseguenza
        ShouldBeSet and (is_integer(LastLabel)) ->
            %%% A final state is a state with the final tag in the label
            FormattedLabel = ?FINALTAG ++ integer_to_list(LastLabel),
            digraph:add_vertex(Graph, Vertex, FormattedLabel);
        true ->
            ?UNDEFINED
    end.

ltoa(L) when is_list(L) -> list_to_atom(L);
ltoa(L) when is_atom(L) -> L.
atol(A) when is_atom(A) -> atom_to_list(A);
atol(A) when is_list(A) -> A.
