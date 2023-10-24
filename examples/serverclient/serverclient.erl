-module(serverclient).
-export([main/0, client/0, server/0, handle_req/1]).

client() ->
    server ! {req, self()},
    receive
        {res, Handle} ->
            cli_loop(Handle)
    end.

cli_loop(Handle) ->
    WantMore = true,
    case WantMore of
        true ->
            Handle ! next,
            cli_loop(Handle);
        false ->
            Handle ! done
    end.

server() ->
    receive
        {req, P} ->
            H = spawn(?MODULE, handle_req, [P]),
            P ! {res, H},
            server();
        ciao ->
            spawn(?MODULE, handle_req, [self()])
    end.

handle_req(C) ->
    receive
        next -> handle_req(C);
        done -> done
    end.

main() ->
    S = spawn(?MODULE, server, []),
    register(server, S),
    spawn(?MODULE, client, []),
    done.
