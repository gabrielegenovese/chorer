-module(unknown).
-export([main/0]).

main() ->
    counter(5).

counter(N) ->
    S = self(),
    S ! N,
    receive
        0 -> done;
        _ -> counter(N - 1)
    end.
