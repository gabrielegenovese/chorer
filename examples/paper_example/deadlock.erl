-module(deadlock).
-export([main/0, server/0]).

main() ->
    S = spawn(?MODULE, server, []),
    N = rand:uniform(10),
    case N > 8 of
        true ->
            S ! {self(), req},
            receive
                data -> ok
            end;
        false ->
            none
    end,
    S ! stop.

server() ->
    receive
        stop ->
            ok;
        {P, req} ->
            P ! somedata,
            server()
    end.
