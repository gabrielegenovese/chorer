-module(simple).
-export([stop_tick/0, loop/0, tick/0, tack/0]).

tack() ->
    receive
        tick ->
            io:fwrite("Received tick~n"),
            tick ! tack,
            tack();
        stop ->
            io:fwrite("Process ended~n")
    end.

tick() ->
    spawn_process(),
    tack ! tick.

spawn_process() ->
    Tack = spawn(?MODULE, tack, []),
    register(tack, Tack),
    Loop = spawn(?MODULE, loop, []),
    spawn(?MODULE, random, []),
    register(tick, Loop).

loop() ->
    receive
        tack ->
            io:fwrite("Received tack~n"),
            tack ! tick,
            loop();
        stop ->
            io:fwrite("Process ended~n"),
            tack ! stop
    end.

stop_tick() ->
    tick ! stop.

random() ->
    done.
