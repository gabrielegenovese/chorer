-module(simple).
-export([dummy/1]).

dummy([]) ->
    done;
dummy(N) ->
    case N of
        a ->
            spawn(?MODULE, main, []);
        b ->
            dummy(N);
        _ ->
            N ! "ciao"
    end,
    send(N, "ciao"),
    receive
        a ->
            N ! ciao;
        c ->
            dummy(N);
        b ->
            N ! addio
    end.

send(A, Data) ->
    A ! Data.
