-module(simple).
-export([tick/0, tack/1]).

tack(Tick) ->
    receive
        tick ->
            Tick ! tack,
            tack(Tick);
        stop ->
            done
    end.

tick() ->
    Tack = spawn(?MODULE, tack, [self()]),
    loop(Tack),
    % errore nel produrre il grafo se questa linea viene decommentanta
    Tack ! stop.

loop(Tack) ->
    Tack ! tick,
    receive
        tick ->
            loop(Tack);
        stop ->
            done
    end.
