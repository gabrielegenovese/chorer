-module(hello).
-export([greet/1]).

greet(Who) ->
    io:fwrite("Hello ~p!~n", [Who]).
