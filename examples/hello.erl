-module(hello).
-export([main/0, dummy1/0, dummy2/0]).

dummy1() ->
    receive
        ciao -> done
    end.

dummy2() ->
    receive
        ciao -> done
    end.

main() ->
    A = spawn(?MODULE, dummy1, []),
    B = spawn(?MODULE, dummy2, []),
    A ! ciao,
    B ! ciao.
