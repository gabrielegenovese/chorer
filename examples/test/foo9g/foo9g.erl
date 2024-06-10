-module(foo9g).
-export([main/0,f/1,g/2,h/2,foo/0]).

main() -> 
    S = spawn(foo9g,foo,[]),
    f(S), 
    S ! ok.

f(S) -> g(S,1),h(S,1).

g(S,N) -> case N of
            0 -> spawn(foo9g,foo,[]), g(S,1), S ! msg3;
            1 -> S ! msg1
          end,
          S.

h(S,N) -> case N of
            0 -> S ! msg2, h(S,1), S ! msg4;
            1 -> spawn(foo9g,foo,[]) 
          end.

foo() -> receive
            ok -> done;
            _ -> foo()
         end.