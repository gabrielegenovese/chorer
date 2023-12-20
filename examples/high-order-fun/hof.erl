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
    A = spawn(fun() -> recv() end), %% TODO: implementare questa feature
    A ! hello,
    Fun().
