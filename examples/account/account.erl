-module(account).
-export([main/0, account/1, client/1]).

main() ->
    Acc = spawn(?MODULE, account, [1]),
    spawn(?MODULE, client, [Acc]),
    spawn(?MODULE, client, [Acc]).

account(Value) ->
    receive
        {get, P} ->
            P ! Value,
            account(value);
        {set, NewValue} ->
            account(NewValue)
    end.

client(Account) ->
    Account ! {get, self()},
    receive
        _Val -> do_something
    end,
    Account ! {set, 42}.
