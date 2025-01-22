-module(airline).
-export([main/0, agent/1]).

main() ->
    spawn(?MODULE, agent, [self()]),
    spawn(?MODULE, agent, [self()]),
    seats(3).

seats(0) ->
    stop;
seats(Num) ->
    receive
        {sell, Pid1} ->
            Pid1 ! {booked, Num},
            seats(Num - 1)
    end.

agent(Pid2) ->
    Pid2 ! {sell, self()},
    receive
        {booked, _} ->
            agent(Pid2)
    end.
