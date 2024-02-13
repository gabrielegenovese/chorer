-module(airline).
-export([main/0, agent/1]).

main() ->
    spawn(?MODULE, agent, [self()]),
    spawn(?MODULE, agent, [self()]),
    seats(self(), 3).

seats(Pid, Num) ->
    Pid ! 2,
    receive
        {sell, Pid1} ->
            Pid1 ! {booked, Num},
            seats(Pid, Num - 1)
    end.

agent(Pid2) ->
    Pid2 ! {sell, self()},
    receive
        {booked, _} ->
            agent(Pid2)
    end.
