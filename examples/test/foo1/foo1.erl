-module(foo1).
-export([test/0,b/0,c/1]).

test() -> B = spawn(foo1,b,[]),
          C = spawn(foo1,c,[B]),
          B ! uno,
          C ! dos.

c(B) -> receive X -> B ! X end.

b() -> receive X -> X end.
