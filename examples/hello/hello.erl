-module(hello).
-export([greet/0]).

greet() ->
    Me = self(),
    Me ! hello1,
    if
        true ->
            Me ! hello2,
            greet();
        false ->
            receive
                hello1 -> done;
                hello2 -> done
            end
    end.
