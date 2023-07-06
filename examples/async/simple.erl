-module(simple).
-export([main/0, dummy1/0, dummy2/0]).

dummy1() ->
    d2 ! ciao,
    receive
        ciao -> done
    end.

dummy2() ->
    d1 ! ciao,
    receive
        ciao -> done
    end.

main() ->
    A = spawn(?MODULE, dummy1, []),
    register(d1, A),
    B = spawn(?MODULE, dummy2, []),
    register(d2, B).
