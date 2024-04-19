-module(trick).
-export([main/0, a/2, b/1, c/0]).

a(B, C) ->
    C ! v1,
    B ! v2.

b(C) ->
    receive
        v2 -> C ! v2
    end.

c() ->
    receive
        v1 -> done
    end,
    receive
        v2 -> done
    end.

main() ->
    C = spawn(?MODULE, c, []),
    B = spawn(?MODULE, b, [C]),
    spawn(?MODULE, a, [B, C]).