-module(airline).
-export([main/0, agent/2]).

main() ->
    spawn(?MODULE, agent, [self(), 1]),
    seats(3).

seats(Num) ->
    receive
        {numOfSeats, Pid1} ->
            Pid1 ! {seats, Num},
            seats(Num);
        {sell, Pid2} ->
            io:format("Seat sold!~n"),
            Pid2 ! {booked, Num},
            seats(Num - 1)
    end.

agent(Pid3, NAg) ->
    Pid3 ! {numOfSeats, self()},
    receive
        {seats, Num} when Num > 0 ->
            Pid3 ! {sell, self()},
            receive
                {booked, _} -> agent(Pid3, NAg)
            end;
        _ ->
            io:format("Agent~p done!~n", [NAg])
    end.