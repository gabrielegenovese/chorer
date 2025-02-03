-module(log).
-include("common_data.hrl").

%%% API
-export([
    info/2,
    debug/2,
    warning/4,
    warning/5,
    error/2,
    error/3
]).

%%%===================================================================
%%% API
%%%===================================================================

info(String, Content) ->
    io:fwrite("[INFO] " ++ String, Content).

debug(String, Content) ->
    io:fwrite("[INFO] " ++ String, Content).

warning(Package, String, Content, RetData) ->
    io:fwrite("[WARNING][~s] " ++ String ++ " ~p~n", [Package, Content]),
    case Package of
        "GV" -> RetData;
        _ -> RetData#localview{ret_var = #variable{}}
    end.
warning(Package, String, Content, RetData, line) ->
    [{_, Line}] = ets:lookup(?CLINE, line),
    io:fwrite("[WARNING][~s] On line ~p: " ++ String ++ " ~p~n", [Package, Line, Content]),
    case Package of
        "GV" -> RetData;
        _ -> RetData#localview{ret_var = #variable{}}
    end.

error(String, Content) ->
    io:fwrite("[ERROR]" ++ String, Content).
error(String, Content, RetData) ->
    io:fwrite("ERROR: ~p ~p~n", [String, Content]),
    RetData.
