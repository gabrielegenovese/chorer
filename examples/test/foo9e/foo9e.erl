-module(foo9e).
-export([main/0,f/1,g/2,h/2,foo/0]).

main() -> 
    S = spawn(foo9e,foo,[]),
    f(S), 
    S ! ok.

f(S) -> g(S,1), h(S,0).

g(S,N) -> case N of
            0 -> spawn(foo9e,foo,[]);
            1 -> S ! msg1 
          end.

h(S,N) -> case N of
            0 -> S ! msg2;
            1 -> spawn(foo9e,foo,[]) 
          end.

foo() -> receive
            ok -> done;
            _ -> foo()
         end.