-module(foo6).
-export([test/0,client_server/1,server/0,client_gen/2,client/1]).

test() -> 
    client_server(5).

client_server(N) ->
    S = spawn(foo6,server,[]),
    client_gen(S,N).

server() ->
    receive 
        X -> ok
    end,
    server().

client_gen(S,1) -> 
    spawn(foo6,client,[S]);
client_gen(S,N) when N>1 -> 
    spawn(foo6,client,[S]), 
    client_gen(S,N-1).

client(S) ->
    S ! hello.