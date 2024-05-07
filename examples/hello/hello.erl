-module(hello).
-export([greet/0, dummy/0]).

dummy() ->
    D = spawn(fun() -> receive X -> X end end),
    D ! hello.
    % non supportare spawn(fun() -> receive X -> X end end) ! hello.

greet() ->
    self() ! 1,
    T = spawn(?MODULE, dummy, []),
    S = spawn(?MODULE, dummy, []),
    receive
        X -> X
    end.
 