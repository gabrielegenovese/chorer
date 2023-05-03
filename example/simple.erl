-module(simple).
-export([tick/0, tack/1]).

tack(Tick) ->
    receive
        tick ->
            Tick ! tack
    end,
    tack(Tick).

tick() ->
    Tack = spawn(?MODULE, tack, [self()]),
    loop(Tack).
    % Tack ! something. % errore nel produrre il grafo se questa linea viene decommentanta

loop(Tack) ->
    Tack ! tick,
    receive
        tick ->
            loop(Tack)
    end.
