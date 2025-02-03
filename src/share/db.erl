%%%===================================================================
%%% @doc
%%% Helper module for managing the internal database.
%%% @end
%%%===================================================================
-module(db).
-include("common_data.hrl").

%%% API
-export([
    init/1,
    close/0,
    get_fun_ast/1,
    get_localview/1,
    get_lv_graph/1,
    get_lv_edge_additonal_info/1,
    inc_spawn_counter/1
]).

-define(DB_NAMESL, [?DBMANAGER, ?CLINE, ?FUNAST, ?LOCALVIEW, ?REGISTERDB, ?ARGUMENTS, ?SPAWNCOUNT]).

%%%===================================================================
%%% API
%%%===================================================================

init(Settings) ->
    lists:foreach(
        fun(DbName) -> ets:new(DbName, [set, named_table]) end,
        ?DB_NAMESL
    ),
    ets:insert(?DBMANAGER, {?SETTINGS, Settings}).

close() ->
    lists:foreach(
        fun(DbName) -> ets:delete(DbName) end,
        ?DB_NAMESL
    ).

get_fun_ast(FunName) ->
    lookup(?FUNAST, share:atol(FunName)).

get_localview(FunName) ->
    lookup(?LOCALVIEW, share:atol(FunName)).

get_lv_graph(FunName) ->
    LV = get_localview(FunName),
    LV#localview.graph.

get_lv_edge_additonal_info(FunName) ->
    LV = get_localview(FunName),
    LV#localview.edge_additional_info.

inc_spawn_counter(Name) ->
    Ret = lookup(?SPAWNCOUNT, share:ltoa(Name)),
    Val =
        case Ret of
            not_found -> 0;
            N -> N
        end,
    ets:insert(?SPAWNCOUNT, {Name, Val + 1}),
    Val.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

lookup(DB, Data) ->
    Ret = ets:lookup(DB, Data),
    case Ret of
        [] ->
            % io:fwrite("[S] Not Found in db ~p for data ~p~n", [DB, Data]),
            not_found;
        [{_, D}] ->
            D
    end.
