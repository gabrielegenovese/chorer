%%%-------------------------------------------------------------------
%%% @doc
%%% This module generate a localview for each possible actor.
%%% Must be used after `md:extract'.
%%% @end
%%%-------------------------------------------------------------------
-module(lv).
-include("../share/common_data.hrl").

%%% API
-export([generate/1, create_localview/3, eval_codeline/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% A localview is generated for each possible actor.
%%% `md:extract' must be used before this function.
generate(Settings) ->
    ActorList = get_actors(),
    lists:foreach(
        fun(Actor) -> create_localview(Actor, Settings, true) end,
        ActorList
    ).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

get_actors() ->
    [{_, ActorList}] = ets:lookup(?DBMANAGER, ?ACTORLIST),
    ActorList.

create_localview(ActorName, Settings, Save) ->
    case does_actor_exist(ActorName) of
        false ->
            io:fwrite("Error: Actor ~p's AST not found~n", [ActorName]),
            no_graph;
        ActorAst ->
            LV = share:get_localview(ActorName),
            case LV of
                not_found ->
                    io:fwrite("[LV] Creating a localview for ~p~n", [ActorName]),
                    BaseData = #localview{
                        fun_name = ActorName, fun_ast = ActorAst, settings = Settings
                    },
                    share:add_vertex(BaseData#localview.graph),
                    LVData = eval_codeline(BaseData#localview.fun_ast, BaseData),
                    G = LVData#localview.graph,
                    % this operation MUST be before the minimize
                    set_final_state(G),
                    MinG = fsa:minimize(G),
                    NewLV = LVData#localview{min_graph = MinG},
                    ets:insert(?LOCALVIEW, {ActorName, NewLV}),
                    case Save or Settings#setting.save_all of
                        true -> share:save_graph(NewLV, Settings, ActorName, local);
                        false -> done
                    end,
                    NewLV;
                L ->
                    L
            end
    end.

does_actor_exist(ActorName) ->
    ActorAst = share:get_fun_ast(share:atol(ActorName)),
    case ActorAst of
        not_found -> false;
        A -> A
    end.

eval_codeline(CodeLine, Data) ->
    debug_print(CodeLine),
    case CodeLine of
        {function, _, DefinitionList} -> eval:function_list(DefinitionList, Data);
        % Do NOT uncomment: this should never match
        % {clause, _, Vars, Guard, Content} -> eval:clause(Content, Vars, Guard, Data, "");
        {match, _, RightContent, LeftContent} -> eval:match(RightContent, LeftContent, Data);
        {call, _, Function, ArgList} -> eval:function_call(Function, ArgList, Data);
        {'case', _, Content, PMList} -> eval:case_pm(Content, PMList, Data);
        {'if', _, PMList} -> eval:if_pm(PMList, Data);
        {'receive', _, PMList} -> eval:receive_pm(PMList, Data);
        {op, _, Op, LeftC, RightC} -> eval:operation(Op, LeftC, RightC, Data);
        {'fun', N, Ast} -> eval:anon_function(Ast, N, Data);
        {integer, _, Val} -> eval:simple_type(integer, Val, Data);
        {float, _, Val} -> eval:simple_type(float, Val, Data);
        {string, _, Val} -> eval:simple_type(string, Val, Data);
        {nil, _} -> eval:simple_type(nil, [], Data);
        {atom, _, Val} -> eval:atom(Val, Data);
        {cons, _, HeadList, TailList} -> eval:list(HeadList, TailList, Data);
        {map, _, Val} -> eval:map(Val, Data);
        {tuple, _, Val} -> eval:tuple(Val, Data);
        {var, _, VarName} -> eval:variable(VarName, Data);
        % attention: don't set this to eval:list([], [], Data) otherwise infinite loop
        [] -> eval:simple_type(list, [], Data);
        [H | T] -> eval:list(H, T, Data);
        _ -> share:warning("couldn't parse code line", CodeLine, Data)
    end.

debug_print(CodeLine) ->
    if
        is_tuple(CodeLine) ->
            Line = element(2, CodeLine),
            % io:fwrite("Evaluating ~p on line ~p~n", [element(1, CodeLine), Line]),
            ets:insert(?CLINE, {line, Line});
        true ->
            done
    end.

%%% Set vertices as a final state if they do not have out edges
set_final_state(Graph) ->
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
