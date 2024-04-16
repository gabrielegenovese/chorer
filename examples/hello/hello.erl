-module(hello).
-export([greet/0]).

greet() ->
    Me = self(),
    Me ! hello1,
    Bool = io:fread("~p"),
    if
        Bool ->
            Me ! hello2,
            greet();
        not Bool ->
            receive
                hello1 -> done;
                hello2 -> done
            end
    end.
