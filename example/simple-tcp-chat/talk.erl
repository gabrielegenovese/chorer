-module(talk).
-compile(export_all).

start(LocalPort, RemotePort) ->
    ServerPid = spawn(?MODULE, start_server, [LocalPort]),
    cli(RemotePort, ServerPid).

cli(RemotePort, ServerPid) ->
    Data = clean(io:get_line('Message: ')),
    case should_stop(Data) of
        true ->
            ServerPid ! done;
        false ->
            spawn(?MODULE, send, [RemotePort, Data]),
            cli(RemotePort, ServerPid)
    end.

clean(Data) ->
    string:strip(Data, both, $\n).

should_stop(Str) ->
    0 =:= string:len(Str).

start_server(Port) ->
    case
        gen_tcp:listen(Port, [
            binary,
            {packet, 4},
            {reuseaddr, true},
            {active, true}
        ])
    of
        {ok, Listen} ->
            server(Listen);
        {error, Reason} ->
            {error, Reason}
    end.

server(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            server_loop(Socket),
            server(Listen);
        _ ->
            ok
    end.

server_loop(Socket) ->
    receive
        done ->
            gen_tcp:close(Socket),
            io:format("Server socket closed~n");
        {tcp, Socket, Bin} ->
            Str = clean(binary_to_term(Bin)),
            io:format("~p~n", [Str]),
            case should_stop(Str) of
                true ->
                    void;
                false ->
                    server_loop(Socket)
            end;
        {tcp_closed, _Socket} ->
            ok
    end.

send(Port, Data) ->
    case gen_tcp:connect("localhost", Port, [binary, {packet, 4}]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, term_to_binary(Data)),
            gen_tcp:close(Socket);
        {error, Reason} ->
            {error, Reason}
    end.
