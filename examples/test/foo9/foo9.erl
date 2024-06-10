-module(foo9).
-export([main/0,f/1,g/1,foo/0]).

main() -> 
    f(g(3)).

g(N) when N>0 -> g(N-1);
g(0) -> spawn(foo9,foo,[]).

f(Pid) -> Pid ! ok.

foo() -> receive
            ok -> ok
         end.