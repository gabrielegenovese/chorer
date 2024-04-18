-module(airline).
-export([main/0, agent/2]).

main() ->
    Main = self(),
    spawn(?MODULE, agent, [1, Main]),
    seats(3).

seats(Num) ->
    receive
        {numOfSeats, Pid} ->
            Pid ! {seats, Num},
            seats(Num);
        {sell, Pid} ->
            io:format("Seat sold!~n"),
            Pid ! {booked, Num},
            seats(Num - 1);
        stop ->
            done
    end.

agent(NAg, Pid) ->
    Pid ! {numOfSeats, self()},
    receive
        {seats, Num} when Num > 0 ->
            Pid ! {sell, self()},
            receive
                {booked, _} -> agent(NAg, Pid)
            end;
        {seats, 0} ->
            Pid ! stop
    end.
