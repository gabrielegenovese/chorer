-module(conditional_case).
-export([main/0, dummy/1]).

dummy(Pid) ->
    Pid ! {self(), rand:uniform()},
    receive
        Str -> io:fwrite("~s~n", [Str])
    end.

main() ->
    A = spawn(?MODULE, dummy, [self()]),
    B = spawn(?MODULE, dummy, [self()]),
    C = spawn(?MODULE, dummy, [self()]),
    D = spawn(?MODULE, dummy, [self()]),

    P =
        receive
            {Process, _} -> Process
        end,

    case P of
        A -> A ! "Ciao A";
        B -> B ! "Ciao B";
        C -> C ! "Ciao C";
        D -> D ! "Ciao D";
        true -> io:fwrite("Boh~n")
    end,

    receive
        Obj -> Obj
    end.
