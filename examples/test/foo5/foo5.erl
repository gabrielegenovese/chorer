-module(foo5).
-export([test/0,proxy/1,target/0]).

test() -> Target = spawn(foo5,target,[]),
          Proxy1 = spawn(foo5,proxy,[Target]),
          Proxy2 = spawn(foo5,proxy,[Target]),
          Target ! m1,
          Proxy1 ! m2,
          Proxy2 ! m3,
          Target ! m4.

proxy(Target) -> receive M -> Target ! M end. 

target() -> receive A -> A end,
            receive B -> B end,
            receive C -> C end,
            receive D -> D end,
             io:format("Output: ~p-~p-~p-~p~n",[A,B,C,D]).
