-module(simple).
-export([tickl/0, tick/0, tack/0, random/0]).

tack() ->
    receive
        tick ->
            io:fwrite("Received tick~n"),
            ptick ! tack,
            tack();
        stop ->
            io:fwrite("Process ended~n")
    end.

tick() ->
    spawn_process(),
    ptack ! tick,
    started.

spawn_process() ->
    Tack = spawn(?MODULE, tack, []),
    register(ptack, Tack),
    TLoop = spawn(?MODULE, tickl, []),
    register(ptick, TLoop),
    spawn(?MODULE, random, []).

tickl() ->
    receive
        tack ->
            io:fwrite("Received tack~n"),
            ptack ! tick,
            tickl();
        stop ->
            io:fwrite("Process ended~n"),
            ptack ! stop
    end.

random() ->
    RandInt = rand:uniform(10),
    io:fwrite("~p~n", [RandInt]),
    if
        RandInt > 9 -> ptick ! stop;
        true -> random()
    end.
