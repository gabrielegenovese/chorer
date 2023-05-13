-module(map_manager).

%%% Api
-export([loop/0]).

%%%===================================================================
%%% API
%%%===================================================================

loop() ->
    % init empty maps
    loop(#{}, #{}, #{}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

loop(CommonMap, FunAstMap, GraphMap) ->
    receive
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Common Map Operations %%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%==========================
        {set_entrypoint, Ep} ->
            NewCommon = maps:put(entrypoint, Ep, CommonMap),
            loop(NewCommon, FunAstMap, GraphMap);
        {P, get_entrypoint} ->
            P ! {maps:get(entrypoint, CommonMap)},
            loop(CommonMap, FunAstMap, GraphMap);
        %%%==========================
        {set_exported_fun, L} ->
            NewCommon = maps:put(exported_fun, L, CommonMap),
            loop(NewCommon, FunAstMap, GraphMap);
        {P, get_exported_fun} ->
            P ! {maps:get(exported_fun, CommonMap)},
            loop(CommonMap, FunAstMap, GraphMap);
        %%%==========================
        {set_ast, Ast} ->
            NewCommon = maps:put(ast, Ast, CommonMap),
            loop(NewCommon, FunAstMap, GraphMap);
        {P, get_ast} ->
            P ! {maps:get(ast, CommonMap)},
            loop(CommonMap, FunAstMap, GraphMap);
        %%%==========================
        {set_actor_list, ActorList} ->
            NewCommon = maps:put(actor_list, ActorList, CommonMap),
            loop(NewCommon, FunAstMap, GraphMap);
        {P, get_actor_list} ->
            P ! {maps:get(actor_list, CommonMap)},
            loop(CommonMap, FunAstMap, GraphMap);
        %%%==========================

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Function Map Operations %%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%==========================
        %%% function asts map getter
        {P, get_ast_map} ->
            P ! {FunAstMap},
            loop(CommonMap, FunAstMap, GraphMap);
        %%% function asts element map setter and getter
        {set_fun_ast, Key, Ast} ->
            NewFunAst = maps:put(Key, Ast, FunAstMap),
            loop(CommonMap, NewFunAst, GraphMap);
        {P, get_fun_ast, Key} ->
            IsKey = lists:member(Key, maps:keys(FunAstMap)),
            if
                IsKey ->
                    P ! {maps:get(Key, FunAstMap)};
                not IsKey ->
                    % io:fwrite("No ast found for ~p~n", [Key]),
                    P ! {no_ast_found}
            end,
            loop(CommonMap, FunAstMap, GraphMap);
        %%%==========================

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Graph Map Operations %%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%==========================
        %%% function graphs map getter
        {P, get_graph_map} ->
            P ! {GraphMap},
            loop(CommonMap, FunAstMap, GraphMap);
        %%% getter and setter for function graphs elements
        {set_fun_graph, Key, Ast} ->
            NewGraphMap = maps:put(Key, Ast, GraphMap),
            loop(CommonMap, FunAstMap, NewGraphMap);
        {P, get_fun_graph, Key} ->
            IsKey = lists:member(Key, maps:keys(GraphMap)),
            if
                IsKey ->
                    P ! {maps:get(Key, GraphMap)};
                not IsKey ->
                    io:fwrite("No graph found for ~p~n", [Key]),
                    P ! {no_graph_found}
            end,
            loop(CommonMap, FunAstMap, GraphMap);
        %%%==========================
        _ ->
            no_pattern_matching_found
    end.
