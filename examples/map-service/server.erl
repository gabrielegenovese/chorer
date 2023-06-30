-module(server).
-export([start/0, loop/1]).

start() ->
    spawn(server, loop, [maps:new()]).

loop(Map) ->
    receive
        {Client, {get, Key}} ->
            Client ! {self(), maps:find(Key, Map)},
            loop(Map);
        {Client, {set, Key, Value}} ->
            UpdatedMap = maps:put(Key, Value, Map),
            Client ! {self(), {ok, Key, Value}},
            loop(UpdatedMap);
        {Client, {size}} ->
            Client ! {self(), {ok, maps:size(Map)}},
            loop(Map);
        {Client, {keys}} ->
            Client ! {self(), {ok, maps:keys(Map)}},
            loop(Map);
        {Client, {delete, Key}} ->
            % check if key exist
            Check = maps:find(Key, Map),
            case Check of
                {error} ->
                    Client ! {self(), {keyNotExist}},
                    loop(Map);
                {ok, Value} ->
                    UpdatedMap = maps:remove(Key, Map),
                    Client ! {self(), {ok, Value}},
                    loop(UpdatedMap)
            end;
        {Client, {exist, Key}} ->
            Client ! {self(), maps:find(Key, Map)},
            loop(Map)
    end.
