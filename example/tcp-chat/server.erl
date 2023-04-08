-module(server).
-export([start/1]).

start(Port) ->
    io:format("Started on port ~s~n", Port).
