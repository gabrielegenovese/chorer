-module(trick).
-export([main/0, a/0, b/0, c/0]).

a() ->
    cc ! v1,
    bb ! v2.

b() ->
    receive
        v2 -> cc ! v2
    end.

c() ->
    receive
        v1 -> done
    end,
    receive
        v2 -> done
    end.

main() ->
    A = spawn(?MODULE, a, []),
    B = spawn(?MODULE, b, []),
    C = spawn(?MODULE, c, []),
    register(aa, A),
    register(bb, B),
    register(cc, C).
