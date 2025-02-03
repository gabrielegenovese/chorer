-module(dining).
-export([main/0, fork/0, philo/2]).

main() ->
    F1 = spawn(?MODULE, fork, []),
    F2 = spawn(?MODULE, fork, []),
    spawn(?MODULE, philo, [F1, F2]),
    spawn(?MODULE, philo, [F2, F1]).

philo(F1, F2) ->
    F1 ! {self(), req},
    receive
        ok -> wait
    end,
    F2 ! {self(), req},
    receive
        ok -> eat
    end,
    F1 ! {self(), release},
    F2 ! {self(), release},
    philo(F1, F2).

fork() ->
    receive
        {P, req} ->
            P ! ok,
            receive
                {P, release} -> ok
            end
    end,
    fork().
