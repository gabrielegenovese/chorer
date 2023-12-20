-module(foo3).
-export([test/0, a/1, b/1]).

test() ->
    S = self(),
    spawn(foo3, a, [S]),
    spawn(foo3, b, [S]),
    receive
        X -> X
    end,
    receive
        Y -> Y
    end,
    receive
        Z -> Z
    end.

a(S) ->
    S ! 1,
    S ! 3.

b(S) -> S ! 2.
