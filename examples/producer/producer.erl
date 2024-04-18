-module(producer).
-export([main/0, consumer/0, producer/0]).

recv() ->
    receive
        D -> D
    end.

consumer() ->
    S = self(),
    producer ! {req, S},
    recv(),
    producer ! {req, S},
    recv(),
    producer ! {req, S},
    recv(),
    producer ! {req, S},
    recv().

producer() ->
    receive
        {req, P} ->
            P ! item,
            producer()
    end.

main() ->
    Prd = spawn(?MODULE, producer, []),
    register(producer, Prd),
    spawn(?MODULE, consumer, []).
