-module(hello).
-export([main/0, dummy/1]).

main() ->
    dummy(1),
    dummy(2).

dummy(N) ->
    case N of
        1 ->
            M = 3 * N,
            sendrecv(M),
            stop;
        _ ->
            G = 5 * N,
            sendrecv(G),
            dummy(N - 1)
    end.

sendrecv(N) ->
    self() ! N,
    receive
        _ -> stop
    end.
