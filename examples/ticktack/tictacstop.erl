-module(simple).
-export([start/0, random/0, tic_loop/0, tac_loop/0]).

start() ->
    spawn_process(),
    tic_loop ! tac,
    started.

spawn_process() ->
    Tac = spawn(?MODULE, tac_loop, []),
    register(tac_loop, Tac),
    Tic = spawn(?MODULE, tic_loop, []),
    register(tic_loop, Tic),
    spawn(?MODULE, random, []).

tac_loop() ->
    receive
        tic ->
            io:fwrite("Received tic~n"),
            case whereis(tic_loop) of
                'undefined' -> done;
                _ -> tic_loop ! tac
            end,
            tac_loop();
        stop ->
            io:fwrite("Process ended~n")
    end.

tic_loop() ->
    receive
        tac ->
            io:fwrite("Received tac~n"),
            tac_loop ! tic,
            tic_loop();
        stop ->
            io:fwrite("Process ended~n"),
            tac_loop ! stop
    end.

random() ->
    RandInt = rand:uniform(10),
    io:fwrite("~p~n", [RandInt]),
    if
        RandInt > 8 -> tic_loop ! stop;
        true -> random()
    end.
