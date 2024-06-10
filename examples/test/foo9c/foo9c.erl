-module(foo9c).
-export([main/0,f/2,foo/0]).

main() -> 
    S = spawn(foo9c,foo,[]),
    f(S,3),
    S ! ok.

f(S,N) when N>0 -> S ! N, f(f(S,N-1),N-1);
f(S,0) -> S ! 0, S.

foo() -> receive
            ok -> done;
            _ -> foo()
         end.