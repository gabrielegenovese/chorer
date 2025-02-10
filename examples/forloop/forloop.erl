-module(forloop).
-export([main/0, dummy/1]).

dummy(Pid) ->
    Pid ! rand:uniform().

forloop(N) when N > 0 ->
    receive
        Num -> Num
    end,
    forloop(N - 1);
forloop(N) when N =:= 0 -> done.

main() ->
    spawn(?MODULE, dummy, [self()]),
    spawn(?MODULE, dummy, [self()]),
    % deadlock
    forloop(3).
