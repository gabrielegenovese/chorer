-module(simple).
-export([start/0, tac_loop/0, tic_loop/0]).

start() ->
    spawn_process(),
    ticl ! tac. % start the loop

spawn_process() ->
    TacPid = spawn(?MODULE, tac_loop, []),
    register(tacl, TacPid), % registro il pid nell'atom tacl
    TicPid = spawn(?MODULE, tic_loop, []),
    register(ticl, TicPid).
    
tac_loop() ->
    receive
        tic ->
            ticl ! tac,
            tac_loop();
        stop ->
            ticl ! stop,
            io:fwrite("Process tac_loop ended~n")
    end.

tic_loop() ->
    receive
        tac ->
            tacl ! tic,
            tic_loop();
        stop ->
            tacl ! stop,
            io:fwrite("Process tic_loop ended~n")
    end.