-module(foo9b).
-export([main/0, f/1, g/2, foo/0]).

main() ->
    S = spawn(foo9b, foo, []),
    f(g(S, 3)).

g(S, 0) ->
    S ! 0,
    S;
g(S, N) when N > 0 ->
    S ! N,
    g(S, N - 1).

f(Pid) -> Pid ! ok.

foo() ->
    receive
        ok -> done;
        _ -> foo()
    end.
