-module(foo9d).
-export([main/0,f/2,foo/0]).

main() -> 
    S = spawn(foo9d,foo,[]),
    f(S,3).

f(S,N) -> case N of
            0 -> S ! 0;
            M when M>0 -> f(S,N-1), S ! M 
          end.

foo() -> receive
            ok -> done;
            _ -> foo()
         end.