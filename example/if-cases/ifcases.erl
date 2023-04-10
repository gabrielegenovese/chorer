-module(ifcases).
-export([main/0, a/0, b/0, c/0]).

a() ->
    receive
        Pid -> Pid
    end,
    if
        is_pid(Pid) ->
            Pid ! "Hi, i'm A";
        true ->
            a()
    end.

b() ->
    receive
        Pid -> Pid
    end,
    if
        is_pid(Pid) ->
            Pid ! "Hi, i'm B";
        true ->
            b()
    end.

c() ->
    receive
        Pid -> Pid
    end,
    if
        is_pid(Pid) ->
            Pid ! "Hi, i'm C";
        true ->
            c()
    end.

main() ->
    R0 = rand:uniform(),
    R1 = rand:uniform(),
    R2 = rand:uniform(),
    A = spawn(?MODULE, a, []),
    B = spawn(?MODULE, b, []),
    C = spawn(?MODULE, c, []),
    if
        R0 > R1 ->
            io:fwrite("Codition 1~n"),
            A ! B,
            B ! C,
            C ! A;
        R1 > R2 ->
            io:fwrite("Codition 2~n"),
            A ! C,
            C ! B,
            B ! A;
        true ->
            io:fwrite("else~n")
    end.
