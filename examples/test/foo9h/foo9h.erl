-module(foo9h).
-export([main/0,f/1,g/2,foo/0]).

main() -> 
    S = spawn(foo9h,foo,[]),
    f(S), 
    S ! ok.

f(S) -> case g(S,1) of
            0 -> g(S,0), S ! msg0;
            1 -> S ! msg1
        end.


g(S,N) -> case N of
            0 -> spawn(foo9h,foo,[]), 0;
            1 -> S ! msg, 1
          end.

foo() -> receive
            ok -> done;
            _ -> foo()
         end.