-module(server).
-export([start/0, loop/0]).

start() ->
    _Pid = spawn(server, loop, []).

loop() ->
    receive
        % pattern matching
        {Client, {Str, uppercase}} ->
            Client ! {self(), string:to_upper(Str)};
        {Client, {Str, lowercase}} ->
            Client ! {self(), string:to_lower(Str)};
        {Client, {_, _}} ->
            Client ! {self(), "Invalid mode"}
    end,
    % using recursion will result in an infinite loop
    loop().
