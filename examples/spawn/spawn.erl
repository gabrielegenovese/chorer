-module(spawn).
-export([main/0, dummy/0, test/0]).

dummy() ->
    spawn(spawn, test, []),
    receive
        Pid -> Pid ! nice
    end,
    spawn(spawn, test, []),
    spawn(spawn, test, []).

test() ->
    done.

main() ->
    D = spawn(spawn, dummy, []),
    D ! self(),
    spawn(spawn, test, []),
    receive
        _ -> done
    end,
    spawn(spawn, test, []).
