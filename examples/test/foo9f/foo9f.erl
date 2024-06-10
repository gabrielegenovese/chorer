-module(foo9f).
-export([main/0,f/1,g/2,h/2,foo/0]).

main() -> 
    S = spawn(foo9f,foo,[]),
    f(S), 
    S ! ok.

f(S) -> h(g(S,1),0).

g(S,N) -> case N of
            0 -> spawn(foo9f,foo,[]);
            1 -> S ! msg1 
          end,
          S.

h(S,N) -> case N of
            0 -> S ! msg2;
            1 -> spawn(foo9f,foo,[]) 
          end.

foo() -> receive
            ok -> done;
            _ -> foo()
         end.