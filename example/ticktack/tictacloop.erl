-module(simple).
-export([start/0, tack_loop/0, tick_loop/0]).

start() ->
    spawn_process(),
    tickl ! tack. % inizia il loop

spawn_process() ->
    Tack = spawn(?MODULE, tack_loop, []),
    register(tackl, Tack), % registro il pid nell'atom tackl
    TLoop = spawn(?MODULE, tick_loop, []),
    register(tickl, TLoop).
    
tack_loop() ->
    receive
        tick ->
            tickl ! tack,
            tack_loop();
        stop ->
            tickl ! stop,
            io:fwrite("Process tack_loop ended~n")
    end.

tick_loop() ->
    receive
        tack ->
            tackl ! tick,
            tick_loop();
        stop ->
            tackl ! stop,
            io:fwrite("Process tick_loop ended~n")
    end.