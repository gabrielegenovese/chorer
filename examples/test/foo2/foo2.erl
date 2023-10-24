-module(foo2).
-export([test/0,a/2,b/0,c/1]).

test() -> B = spawn(foo2,b,[]),
          C = spawn(foo2,c,[B]),
          spawn(foo2,a,[B,C]).

a(B,C) -> B ! 1, C ! 2, B ! 3.

b() -> receive X -> X end,
       receive Y -> Y end,
       receive Z -> Z end.

c(B) -> receive _ -> B ! 4 end. 
