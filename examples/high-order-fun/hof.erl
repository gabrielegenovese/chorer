-module(hello).
-export([greet/0]).

greet() ->
    Fun = fun() ->
        receive
            hello -> done
        end
    end,
    A = spawn(Fun),
    A ! hello,
    Fun().
