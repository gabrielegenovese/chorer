-module(hello).
-export([greet/0]).

greet() ->
    Me = self(),
    Me ! hello1,
    if
        true ->
            Me ! hello1,
            greet();
        false ->
            receive
                hello1 -> done
            end
    end.
