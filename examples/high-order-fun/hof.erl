-module(hof).
-export([greet/0]).

recv() ->
    receive
        hello -> done
    end.

greet() ->
    Fun = fun() ->
        receive
            hello -> done
        end
    end,
    A = spawn(fun() -> recv() end),
    A ! hello,
    self() ! hello,
    B = spawn(Fun),
    B ! hello,
    recv(),
    Fun().
