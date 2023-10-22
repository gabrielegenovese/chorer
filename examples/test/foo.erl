-module(ping).
-export([start/0,ping/1,pong/0]).


start() -> Pong = spawn(ping,pong,[]),
           S = self(),
           S ! 3, ping(Pong).

ping(Pong) -> receive
             kill -> done;
             N -> Pong ! {self(),N-1}, ping(Pong)
        end.  

pong() -> receive
            {S,0} -> S ! kill;
            {S,N} when N>0 -> S ! N, pong()
        end.