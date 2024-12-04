-module(pass).
-export([main/0, a/1]).

a(D) ->
    receive
        D -> done
    end.

main() ->
    Data = hello,
    Proc = spawn(?MODULE, a, [Data]),
    send(Proc, Data).

send(P, D) ->
    P ! D.
