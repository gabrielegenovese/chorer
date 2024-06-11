-module(foo9d).
-export([main/0,f/2,foo/0]).
 
main() -> 
    S = spawn(foo9d,foo,[]),
    f(S,3).
 
f(S,N) -> case N of
            0 -> S ! stop;
            M when M>0 -> f(S,M-1), S ! msg 
          end.
 
foo() -> receive
            stop-> done;
            _ -> foo()
         end.
