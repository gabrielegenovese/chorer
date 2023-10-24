-module(foo4).
-export([test/0,r/0,w1/1,w2/1]).

test() -> Reg = spawn(foo4,r,[]),
          Reg ! r0,
          W1 = spawn(foo4,w1,[Reg]),
          W2 = spawn(foo4,w2,[Reg]),
          W1 ! w1,
          W2 ! w2.

r() -> receive X -> X end,
       receive Y -> Y end,
       receive Z -> Z end.

w1(Reg) -> receive _X -> Reg ! r1 end.

w2(Reg) -> receive _X -> Reg ! r2 end.

% An example from Tasharofi et al 2012
