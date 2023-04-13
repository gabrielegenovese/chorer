-module(simple).
-export([main/0, dummy/0]).

dummy() -> 
    done.

main() -> 
    _A = spawn(?MODULE, dummy, []),
    spawn(?MODULE, dummy, []).
    