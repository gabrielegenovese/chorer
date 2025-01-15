%code showing a mutual exclusion violation
%the two concurrent increments of the variable managed by var
%may lead to final value 1 instead of 2 under some schedulings

-module(meViolation).
-export([main/0, var/2, incr/1]).

main() ->
    VPid = spawn(?MODULE, var, [0, 0]),
    IPid = spawn(?MODULE, incr, [VPid]),
    VPid ! {r, [self(), IPid]},
    receive
        X -> VPid ! {w, X + 1}
    end.

var(Val, Count) ->
    io:format("Variable value:~b~n", [Val]),
    receive
        {w, NewVal} ->
            var(NewVal, Count - 1);
        {r, PidL} when is_list(PidL) ->
            lists:each(fun(P) -> P ! Val end),
            var(Val, length(PidL))
    end.

incr(VPid) ->
    receive
        X -> VPid ! {w, X + 1}
    end.
