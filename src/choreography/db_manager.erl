-module(db_manager).
-include("../share/common_data.hrl").

%%% Api
-export([
    loop/0,
    get_entrypoint/0,
    get_exported_fun/0,
    get_ast/0,
    get_actors/0,
    get_fun_ast/1,
    get_fun_graph/1,
    get_reg_entry/0,
    get_fun_args/1,
    get_func_arg_map/0,
    get_spawn_info/0,
    get_fun_local_vars/1,
    add_reg_entry/1,
    inc_spawn_counter/1,
    reset_spawn_counter/0,
    send_fun_graph/2,
    send_actor_list/1,
    send_ast/1,
    send_fun_ast/2,
    add_fun_arg/2,
    send_spawn_info/1,
    add_fun_local_vars/2
]).

-record(loop_data, {
    common = #{register_list => []}, func_data_m = #{}, func_arg_m = #{}
}).

%%% Function data structure for the map
-record(func_data, {
    is_actor = false,
    ast = no_ast_found,
    graph = no_graph_found,
    spawned_counter = 0,
    local_vars = []
}).

%%%===================================================================
%%% API
%%%===================================================================

%%% init data
loop() -> loop(#loop_data{}).

%%%===================================================================
%%% API to interract with the dbmanager
%%%===================================================================

%%% Getters
get_entrypoint() -> get_from_db({self(), get_entrypoint}).
get_ast() -> get_from_db({self(), get_ast}).
get_exported_fun() -> get_from_db({self(), get_exported_fun}).
get_actors() -> get_from_db({self(), get_actor_list}).
get_fun_ast(Key) -> get_from_db({self(), get_fun_ast, Key}).
get_fun_graph(Key) -> get_from_db({self(), get_fun_graph, Key}).
get_reg_entry() -> get_from_db({self(), get_register_list}).
get_fun_args(K) -> get_from_db({self(), get_fun_arg, K}).
get_spawn_info() -> get_from_db({self(), get_spawn_info}).
get_func_arg_map() -> get_from_db({self(), get_func_args_map}).
get_fun_local_vars(Key) -> get_from_db({self(), get_local_vars, Key}).

%%% Both getter and setter
inc_spawn_counter(Key) -> get_from_db({self(), inc_spawned_counter, Key}).
reset_spawn_counter() -> send_to_db({reset_spawned_counter}).

%%% Setters
send_spawn_info(L) -> send_to_db({set_spawn_info, L}).
send_fun_graph(Key, G) -> send_to_db({set_fun_graph, Key, G}).
send_fun_ast(Key, Ast) -> send_to_db({set_fun_ast, Key, Ast}).
send_actor_list(L) -> send_to_db({set_actor_list, L}).
send_ast(AST) -> send_to_db({set_ast, AST}).
add_reg_entry(L) -> send_to_db({add_to_register_list, L}).
add_fun_arg(Key, Args) -> send_to_db({add_fun_args, Key, Args}).
add_fun_local_vars(Key, L) -> send_to_db({add_local_vars, Key, L}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%% Sent and receive data from the db menager
get_from_db(Data) ->
    send_to_db(Data),
    recv().

%%% Send data to the db menager
send_to_db(Data) ->
    ?DBMANAGER ! Data.

%%% Receive data from the db menager
recv() ->
    receive
        {D} -> D
    end.

loop(Data) ->
    CommonMap = Data#loop_data.common,
    FuncDataM = Data#loop_data.func_data_m,
    FuncArgM = Data#loop_data.func_arg_m,
    receive
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Common Map Operations %%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%==========================
        {set_entrypoint, Ep} ->
            NewCommon = maps:put(entrypoint, Ep, CommonMap),
            loop(Data#loop_data{common = NewCommon});
        {P, get_entrypoint} ->
            P ! {maps:get(entrypoint, CommonMap)},
            loop(Data);
        %%%==========================
        {set_exported_fun, L} ->
            NewCommon = maps:put(exported_fun, L, CommonMap),
            loop(Data#loop_data{common = NewCommon});
        {P, get_exported_fun} ->
            P ! {maps:get(exported_fun, CommonMap)},
            loop(Data);
        %%%==========================
        {set_ast, Ast} ->
            NewCommon = maps:put(ast, Ast, CommonMap),
            loop(Data#loop_data{common = NewCommon});
        {P, get_ast} ->
            P ! {maps:get(ast, CommonMap)},
            loop(Data);
        %%%==========================
        {set_actor_list, ActorL} ->
            NewCommon = maps:put(actor_list, ActorL, CommonMap),
            NewFD = lists:foldl(
                fun(Actor, AccM) ->
                    D = maps:get(Actor, AccM),
                    maps:put(Actor, D#func_data{is_actor = true}, AccM)
                end,
                FuncDataM,
                ActorL
            ),
            loop(Data#loop_data{common = NewCommon, func_data_m = NewFD});
        {P, get_actor_list} ->
            P ! {maps:get(actor_list, CommonMap)},
            loop(Data);
        %%%==========================
        {add_to_register_list, Item} ->
            RegList = maps:get(register_list, CommonMap),
            NewCommon = maps:put(register_list, RegList ++ [Item], CommonMap),
            loop(Data#loop_data{common = NewCommon});
        {P, get_register_list} ->
            P ! {maps:get(register_list, CommonMap)},
            loop(Data);
        %%%==========================
        {set_spawn_info, List} ->
            NewCommonM = maps:put(spawn_info, List, CommonMap),
            loop(Data#loop_data{common = NewCommonM});
        {P, get_spawn_info} ->
            P ! {maps:get(spawn_info, CommonMap)},
            loop(Data);
        %%%==========================

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Function Map Operations %%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%% function data map getter
        {P, get_func_data_m} ->
            P ! {FuncDataM},
            loop(Data);
        %%% function asts element map setter and getter
        {set_fun_ast, Key, Ast} ->
            FuncD = maps:get(Key, FuncDataM, #func_data{}),
            NewFD = maps:put(Key, FuncD#func_data{ast = Ast}, FuncDataM),
            loop(Data#loop_data{func_data_m = NewFD});
        {P, get_fun_ast, Key} ->
            FuncD = maps:get(Key, FuncDataM, no_key_found),
            case FuncD of
                no_key_found -> P ! {no_func_found};
                D -> P ! {D#func_data.ast}
            end,
            loop(Data);
        %%%==========================

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Graph Map Operations %%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%% getter and setter for function graphs elements
        {set_fun_graph, Key, G} ->
            FuncD = maps:get(Key, FuncDataM),
            NewFD = maps:put(Key, FuncD#func_data{graph = G}, FuncDataM),
            loop(Data#loop_data{func_data_m = NewFD});
        {P, get_fun_graph, Key} ->
            FuncD = maps:get(Key, FuncDataM, #func_data{}),
            P ! {FuncD#func_data.graph},
            loop(Data);
        %%% increments  amd send the spawned counter
        {reset_spawned_counter} ->
            NewM = maps:fold(
                fun(K, V, A) -> maps:put(K, V#func_data{spawned_counter = 0}, A) end,
                #{},
                FuncDataM
            ),
            loop(Data#loop_data{func_data_m = NewM});
        {P, inc_spawned_counter, Key} ->
            FuncD = maps:get(Key, FuncDataM),
            C = FuncD#func_data.spawned_counter,
            P ! {C},
            NewFD = maps:put(Key, FuncD#func_data{spawned_counter = C + 1}, FuncDataM),
            loop(Data#loop_data{func_data_m = NewFD});
        %%%==========================
        {add_local_vars, Key, AddL} ->
            FuncD = maps:get(Key, FuncDataM),
            List = FuncD#func_data.local_vars,
            NewFD = maps:put(Key, FuncD#func_data{local_vars = List ++ AddL}, FuncDataM),
            loop(Data#loop_data{func_data_m = NewFD});
        {P, get_local_vars, Key} ->
            FuncD = maps:get(Key, FuncDataM),
            P ! {FuncD#func_data.local_vars},
            loop(Data);
        %%%==========================

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Func Args Operations %%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%

        {P, get_func_args_map} ->
            P ! {FuncArgM},
            loop(Data);
        %%% getter and setter for function graphs elements
        {add_fun_args, Key, Args} ->
            L = maps:get(Key, FuncArgM, []),
            NewFA = maps:put(Key, L ++ Args, FuncArgM),
            loop(Data#loop_data{func_arg_m = NewFA});
        {P, get_fun_arg, Key} ->
            P ! {maps:get(Key, FuncArgM, no_fun_args_found)},
            loop(Data);
        %%%==========================
        {P, _} ->
            io:fwrite("No pattern matching found for process ~p~n", [P]),
            P ! no_pattern_matching_found,
            loop(Data);
        stop ->
            done;
        _ ->
            io:fwrite("No pattern matching found~n"),
            loop(Data)
    end.
