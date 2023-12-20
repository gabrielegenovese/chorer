-module(foo2).
-export([test/0, a/1, b/0, c/0]).

test() ->
    C = spawn(?MODULE, c, []),
    % register(c, C),
    spawn(?MODULE, a, [C]).

a(C) ->
    C ! 2.

b() ->
    receive
        X -> X
    end,
    receive
        Z -> Z
    end.

c() ->
    receive
        _ -> b ! 4
    end.
