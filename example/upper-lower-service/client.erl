-module(client).
-export([send_string/3]).

send_string(ServerPid, String, Mode) ->
    if
        (Mode =/= uppercase) and (Mode =/= lowercase) ->
            io:fwrite("Invalid mode~n");
        true ->
            io:fwrite("Sending ~s~n", [String]),
            ServerPid ! {self(), {String, Mode}},
            receive
                {_, Str} ->
                    io:format("Server returned ~s~n", [Str])
            end
    end.
