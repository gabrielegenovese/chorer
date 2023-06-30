-module(funny).
-export([main/0, dummy/1]).

recv_dummy() ->
    receive
        {_, Msg} -> 
            io:fwrite("~s~n", [Msg])
    end.

dummy(Pid) -> 
    Pid ! {self(), "Hello from dummy (1)"},
    Pid ! {self(), "Hello from dummy (2)"}.

main() ->
    spawn(?MODULE, dummy, [self()]),
    recv_dummy(),
    recv_dummy().