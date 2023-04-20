-module(simple).
-export([dummy/0, dummy/1, main/0]).

dummy() -> bella.

dummy([]) ->
    done;
dummy(N) when not N ->
    receive
        a -> done;
        b -> dummy(N);
        _ -> spawn(?MODULE, main, [])
    end,
    spawn(?MODULE, main, []),
    N ! hello.

main() -> done.
