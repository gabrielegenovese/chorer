-module(hello).
-export([greet/0]).

greet() ->
    Me = self(),
    Bool = io:fread("~p"),
    if
        Bool ->
            Me ! 1;
        not Bool ->
            Me ! 2
    end,
    Me ! 3,
    receive
        X -> X
    end.
