-module(hello).
-export([greet/0]).

greet() ->
    Me = self(),
    Me ! hello1,
    Me ! hello2,
    receive
        M -> M
    end,
    receive
        N -> N
    end.
