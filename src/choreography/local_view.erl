-module(local_view).
-include("../common/common_data.hrl").

%%% API
-export([generate/1]).

-record(variable, {type = ?UNDEFINED, name = ?UNDEFINED, value = ?UNDEFINED}).

%%%===================================================================
%%% API
%%%===================================================================

%%% Generate a local view for each actor
generate(OutputDir) ->
    ActorList = common_fun:get_actors_from_db(),
    %%% For each actor, create and save the local view
    lists:foreach(
        fun(Actor) -> create_localview(Actor, OutputDir) end,
        ActorList
    ).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%% Create a local view for an actor
create_localview(ActorName, OutputDir) ->
    ActorAst = common_fun:get_fun_ast_from_db(ActorName),
    case ActorAst of
        no_ast_found ->
            io:fwrite("Error: Actor ~p's AST not found", [ActorName]);
        _ ->
            %%% TODO: parametri da chiedere all'utente
            {SetFinalState, SetAdditionalInfo} = {true, false},
            Graph = get_graph(ActorName, ActorAst, SetFinalState, SetAdditionalInfo),
            FileNameStr = atol(ActorName),
            %%% Send the graph to the dbmanager
            MinGraph = fsa:minimize(Graph),
            ?DBMANAGER ! {set_fun_graph, ActorName, MinGraph},
            common_fun:save_graph_to_file(MinGraph, OutputDir, FileNameStr, local)
    end.

get_graph(FunName, Code, SetFinal, SetPm) when is_list(Code) ->
    Gr = digraph:new(),
    VStart = common_fun:add_vertex(Gr),
    lists:foreach(
        fun(Line) ->
            case Line of
                {clause, _, Vars, Guard, Content} ->
                    %%% Show the pattern matching options to view the complete local view
                    VN = add_args_to_graph(Gr, Vars, Guard, VStart, SetPm),
                    VFinal = eval_pm_clause(Content, FunName, Gr, VN, SetPm),
                    set_as_final(SetFinal, Gr, VFinal);
                _ ->
                    ?UNDEFINED
            end
        end,
        Code
    ),
    Gr.

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

%%% This function returns the last vertex added
eval_pm_clause(Code, FunName, Gr, VStart, SetPm) ->
    {_, V, _} = lists:foldl(
        fun(Line, AccData) -> eval_codeline(Line, FunName, Gr, AccData, SetPm) end,
        {#variable{}, VStart, []},
        Code
    ),
    V.

%%% This function evaluate a single line of code and returns the last vertex added to the graph
eval_codeline(CodeLine, FunName, G, AccData, SetPm) ->
    {LastRetVar, VLast, LocalVarL} = AccData,
    case CodeLine of
        %%% In a match code line (Var = ...), we just evaluate the left
        %%% content because it could be a receive, case, if, etc...
        %%% Future TODO: in the right content there could be a vmatch ariable,
        %%% so we can save it and its content type (pid, atom, list, etc...) on the dbmanager
        {match, _, RightContent, LeftContent} ->
            {Var, V, NewL} = eval_codeline(LeftContent, FunName, G, AccData, SetPm),
            NewVarEntry = Var#variable{name = RightContent},
            {Var, V, NewL ++ [NewVarEntry]};
        %%% If there's a recursive call, just add an epsilon edge to the first state
        {call, _, {atom, _, FunName}, _ArgList} ->
            digraph:add_edge(G, VLast, 1, 'ɛ'),
            %%% return the start node TODO: capire se è la mossa giusta da fare
            {#variable{}, 1, LocalVarL};
        %%% Evaluate the spawn function
        {call, _, {atom, _, spawn}, [_, {atom, _, Name}, _ArgList]} ->
            VNew = common_fun:add_vertex(G),
            % todo: refactor
            SLabel = ltoa("spawn " ++ atol(Name)),
            digraph:add_edge(G, VLast, VNew, SLabel),
            {#variable{type = pid}, VNew, LocalVarL};
        %%% Evaluate the register function
        {call, _, {atom, _, register}, [{atom, _, AtomV}, {var, _, VarV}]} ->
            manage_register(LocalVarL, AtomV, VarV),
            AccData;
        %%% Evaluate a generic function
        %%% Attention: don't change the position of this pattern matching branch
        {call, _, {atom, _, Name}, _ArgList} ->
            % TODO capire bene cosa succede e perché funziona (caso più unico che raro)
            NewG = eval_func(Name, SetPm),
            case NewG of
                %%% If the function called is a built-in or a module function,
                %%% then we don't have the Ast. Just return the last added vertex
                no_graph -> AccData;
                %%% If there's no error, then we have a graph, then we need to
                %%% add the graph to the main graph
                _ -> {LastRetVar, merge_graph(G, NewG, VLast), LocalVarL}
            end;
        %%% Evaluate Case and If
        {'case', _, {var, _, Var}, PMList} ->
            BaseLabel = get_base_label(SetPm, atol(Var) ++ " match "),
            {#variable{}, eval_pm(PMList, G, VLast, FunName, BaseLabel, SetPm), LocalVarL};
        {'case', _, _, PMList} ->
            BaseLabel = get_base_label(SetPm, "match smt"),
            {#variable{}, eval_pm(PMList, G, VLast, FunName, BaseLabel, SetPm), LocalVarL};
        {'if', _, PMList} ->
            BaseLabel = get_base_label(SetPm, "if "),
            {#variable{}, eval_pm(PMList, G, VLast, FunName, BaseLabel, SetPm), LocalVarL};
        %%% Evaluate Receive
        {'receive', _, PMList} ->
            {#variable{}, eval_pm(PMList, G, VLast, FunName, "receive ", SetPm), LocalVarL};
        %%% Evaluate Send
        {op, _, '!', {_, _, VarOrAtomName}, DataSentAst} ->
            {Var, _, NewL} = eval_codeline(DataSentAst, FunName, G, AccData, SetPm),
            VNew = common_fun:add_vertex(G),
            DataSent = convert_var_to_string(Var),
            SLabel = "send " ++ DataSent ++ " to " ++ atol(VarOrAtomName),
            digraph:add_edge(G, VLast, VNew, ltoa(SLabel)),
            {#variable{type = atom, value = DataSent}, VNew, NewL};
        %%% Evaluate Types
        {integer, _, Val} ->
            {#variable{type = integer, value = Val}, VLast, LocalVarL};
        {float, _, Val} ->
            {#variable{type = float, value = Val}, VLast, LocalVarL};
        {string, _, Val} ->
            {#variable{type = string, value = Val}, VLast, LocalVarL};
        {atom, _, Val} ->
            {#variable{type = atom, value = Val}, VLast, LocalVarL};
        _ ->
            AccData
    end.

convert_var_to_string(Var) ->
    case Var#variable.type of
        ?UNDEFINED ->
            "something";
        Type ->
            case Var#variable.value of
                ?UNDEFINED ->
                    "some " ++ atol(Type);
                Val ->
                    case Type of
                        integer -> integer_to_list(Val);
                        float -> io_lib:format("~.2f", [Val]);
                        string -> Val;
                        atom -> atol(Val)
                    end
            end
    end.

get_base_label(SetPm, Label) ->
    case SetPm of
        true -> Label;
        false -> "ɛ"
    end.

manage_register(LocalVarL, AtomName, VarName) ->
    VarFound = find_var(LocalVarL, VarName),
    case VarFound of
        not_found ->
            ?UNDEFINED;
        V ->
            Cond = is_type_pid(V#variable.type),
            case Cond of
                false -> ?UNDEFINED;
                true -> common_fun:add_reg_entry_from_db(AtomName)
            end
    end.

find_var([], _) ->
    not_found;
find_var([Var | Tail], Name) ->
    Cond = Var#variable.name =:= Name,
    case Cond of
        true -> Var;
        false -> find_var(Tail, Name)
    end.

is_type_pid(Type) ->
    SL = ltoa(Type),
    IsPid = string:find(SL, "pid"),
    is_list(IsPid).

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

%%% This function returns an FSA graph of the function if the ast exist,
%%% otherwise return error.
eval_func(FuncName, SetPm) ->
    FunAst = common_fun:get_fun_ast_from_db(FuncName),
    case FunAst of
        no_ast_found -> no_graph;
        %%% get the graph but don't set the final state
        _ -> get_graph(FuncName, FunAst, false, SetPm)
    end.

eval_pm(PMList, G, VLast, FunName, Label, SetPm) ->
    VLastList = explore_pm(PMList, G, VLast, FunName, Label, SetPm),
    VRet = common_fun:add_vertex(G),
    add_edges_recursive(G, VLastList, VRet, 'ɛ'),
    VRet.

%%% This function explore every pm's branch and returns the list of last added vertex
explore_pm(PMList, G, VLast, FunName, Base, SetPm) ->
    lists:foldl(
        fun(CodeLine, AddedVertexList) ->
            case CodeLine of
                {clause, _, Vars, Guard, Content} ->
                    IsReceive = is_list(string:find(Base, "receive")),
                    %%% if it's a receive pm, then the label must be written
                    EdLabel =
                        if
                            IsReceive -> format_label_pm_edge(true, Vars, Guard, Base);
                            true -> format_label_pm_edge(SetPm, Vars, Guard, Base)
                        end,
                    VL =
                        if
                            IsReceive or SetPm ->
                                VNew = common_fun:add_vertex(G),
                                digraph:add_edge(G, VLast, VNew, EdLabel),
                                VNew;
                            true ->
                                VLast
                        end,
                    VRet = eval_pm_clause(Content, FunName, G, VL, SetPm),
                    if
                        VRet =:= VL -> AddedVertexList;
                        %%% Ruturn the vertex appended to the accumulator list
                        true -> AddedVertexList ++ [VRet]
                    end;
                _ ->
                    AddedVertexList
            end
        end,
        [],
        PMList
    ).

%%% This function format the Variables with the guards in a label for the FSA
format_label_pm_edge(SetPm, VarList, GuardList, BaseLabel) when is_list(BaseLabel) ->
    RetString =
        if
            SetPm ->
                %%% Very basic functions
                VFun = fun(V, L) ->
                    case V of
                        %%% TODO: add more infos
                        {var, _, '_'} -> L ++ "_";
                        {var, _, Var} -> L ++ atol(Var);
                        {atom, _, Atom} -> L ++ atol(Atom);
                        {nil, _} -> L ++ "null";
                        _ -> L
                    end
                end,
                GFun = fun(G, L) ->
                    case G of
                        %%% Add guards if there's a guard
                        %%% TODO: add more infos
                        {op, _, _, _} -> L ++ " (guards)";
                        _ -> L
                    end
                end,
                VarsString = lists:foldl(fun(V, Acc) -> VFun(V, Acc) end, BaseLabel, VarList),
                lists:foldl(fun(G, Acc) -> GFun(G, Acc) end, VarsString, GuardList);
            not SetPm ->
                "ɛ"
        end,
    %%% Returns an atom because labels are atoms
    ltoa(RetString).

add_edges_recursive(G, VertexList, VertexToLink, Label) ->
    %%% Link every V in the VertexList to the VertexToLink, with a specified label
    [
        digraph:add_edge(G, V, VertexToLink, Label)
     || V <- VertexList,
        %%% exclude the start vertex
        V =/= 1
    ].

% Set a state as a final state
set_as_final(ShouldBeSet, G, V) ->
    {V, LastLabel} = digraph:vertex(G, V),
    if
        %%% we can add *and (LastLabel =/= 1) ->* to the guard bacause there could be problems with loops
        %%% Future TODO: in eval_codeline potrei tornare il vertice e un booleano che rappresenti se la
        %%% funzione sia un loop o meno e settare lo stato finale di conseguenza
        ShouldBeSet and (is_integer(LastLabel)) ->
            %%% A final state is a state with the final tag in the label
            FormattedLabel = ?FINALTAG ++ integer_to_list(LastLabel),
            digraph:add_vertex(G, V, FormattedLabel);
        true ->
            ?UNDEFINED
    end.

ltoa(L) when is_list(L) -> list_to_atom(L);
ltoa(L) -> L.
atol(A) when is_atom(A) -> atom_to_list(A);
atol(A) -> A.
