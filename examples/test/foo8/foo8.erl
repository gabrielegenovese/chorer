-module(foo8).
-export([main/0, f/1, g/1, h/1, foo/0]).

main() ->
    spawn(foo8, f, [3]),
    spawn(foo8, g, [c]),
    spawn(foo8, h, [stop]).

f(N) ->
    case N of
        1 -> f(N);
        2 -> g(c);
        3 -> ok
    end.

g(N) ->
    case N of
        a -> g(N);
        b -> h(stop);
        c -> ok
    end.

h(N) ->
    case N of
        stop -> spawn(foo8, foo, []);
        _X -> h(N)
    end.

foo() -> ok.
