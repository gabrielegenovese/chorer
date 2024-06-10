-module(foo7).
-export([main/0,foo/2,server/0]).

% main() -> 
%     S = spawn(foo7,server,[]),
%     foo(S,1),
%     foo(S,2),
%     foo(S,3).

% foo(S,X) -> case X of
%                 1 -> S ! 1;
%                 2 -> S ! 2;
%                 3 -> S ! 3
%             end.

% server() ->
%     receive
%         1 -> server();
%         2 -> server();
%         3 -> ok
%     end.

-module(foo7).
-export([main/0,foo/2,server/0]).

main() -> 
    S = spawn(foo7,server,[]),
    spawn(foo7,foo,[S,1]),
    spawn(foo7,foo,[S,2]),
    spawn(foo7,foo,[S,3]).

foo(S,X) -> case X of
                1 -> S ! 1;
                2 -> S ! 2;
                3 -> S ! 3
            end.

server() ->
    receive
        1 -> server();
        2 -> server();
        3 -> ok
    end.