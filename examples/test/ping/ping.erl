-module(ping).
-export([start/0, ping/1, pong/0]).

start() ->
    Pong = spawn(ping, pong, []),
    S = self(),
    S ! 3,
    ping(Pong).

ping(Pong) ->
    receive
        kill ->
            done;
        N ->
            Pong ! {self(), N - 1},
            ping(Pong)
    end.

pong() ->
    receive
        {S, 0} ->
            S ! kill;
        {S, N} when N > 0 ->
            S ! N,
            pong()
    end.

% Q: Generates a local view for each function? 
% A: only for exported functions, since all of them can give rise
%    to (potential) actors if used as an argument to spawn
%    --if HO is to be supported, calls from anonymous functions
%    --should also be considered  
% Q: The global view seems to omit some messages.
% A: it might be related to the fact that pids are not
%    registered...
% Q Some values are replaced by "none". Why?
% A: a bug.
