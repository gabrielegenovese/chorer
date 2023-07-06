-module(simple).
-export([main/0, dummy1/0, dummy2/0]).

dummy1() ->
    dd ! ciao,
    receive
        ciao -> done
    end.

dummy2() ->
    db ! ciao,
    receive
        ciao -> done
    end.

main() ->
    A = spawn(?MODULE, dummy1, [1]),
    register(dd, A),
    B = spawn(?MODULE, dummy2, [1]),
    register(db, B).
