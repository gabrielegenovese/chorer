-module(funcall).
-export([main/0, dummy/1]).

recv_dummy() ->
    receive
        {_, Msg} -> 
            io:fwrite("~s~n", [Msg])
    end.

dummy(Pid) -> 
    Pid ! {self(), hello1},
    Pid ! {self(), hello2}.

main() ->
    spawn(?MODULE, dummy, [self()]),
    recv_dummy(),
    recv_dummy().