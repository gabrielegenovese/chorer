%code showing a mutual exclusion violation
%the two concurrent increments of the variable managed by var
%may lead to final value 1 instead of 2 under some schedulings

-module(meViolation).
-export([main/0, var/1, incr/1]).

main() ->
    VPid = spawn(?MODULE, var, [0]),
    VPid ! {w, 1},
    spawn(?MODULE, incr, [VPid]).

var(Val) ->
    io:format("Variable value:~b~n", [Val]),
    receive
        {w, NewVal} -> var(NewVal);
        {r, Pid} -> Pid ! Val
    end,
    var(Val).

incr(VPid) ->
    VPid ! {r, self()},
    receive
        X -> VPid ! {w, X + 1}
    end.
