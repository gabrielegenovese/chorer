-module(simple).
-export([tickl/0, tackl/0, tick/0, random/0]).

tackl() ->
    receive
        tick ->
            io:fwrite("Received tick~n"),
            tickl ! tack,
            tackl();
        stop ->
            io:fwrite("Process ended~n")
    end.

tick() ->
    spawn_process(),
    tickl ! tack,
    started.

spawn_process() ->
    Tack = spawn(?MODULE, tackl, []),
    register(tackl, Tack),
    TLoop = spawn(?MODULE, tickl, []),
    register(tickl, TLoop),
    spawn(?MODULE, random, []).

tickl() ->
    receive
        tack ->
            io:fwrite("Received tack~n"),
            tackl ! tick,
            tickl();
        stop ->
            io:fwrite("Process ended~n"),
            tackl ! stop
    end.

random() ->
    RandInt = rand:uniform(10),
    io:fwrite("~p~n", [RandInt]),
    if
        RandInt > 9 -> tickl ! stop;
        true -> random()
    end.
