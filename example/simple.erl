-module(simple).
-export([dummy/1, main/0]).

dummy([]) ->
    done;
dummy(N) when not N ->
    receive
        a -> done;
        _ -> spawn(?MODULE, main, [])
    end,
    spawn(?MODULE, main, []).

main() -> done.
