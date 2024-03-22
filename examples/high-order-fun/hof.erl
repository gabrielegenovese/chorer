-module(hof).
-export([greet/0]).

recv() ->
    receive
        _ -> done
    end.

greet() ->
    Fun = fun() ->
        receive
            _ -> done
        end
    end,
    A = spawn(fun() -> recv() end),
    A ! hello1,
    self() ! hello2,
    B = spawn(Fun),
    B ! hello3,
    recv(),
    Fun().
