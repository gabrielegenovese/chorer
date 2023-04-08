-module(client).
-export([start/2]).

start(Ip, Port) ->
    io:format("Started on ~s:~s~n", Ip, Port).
