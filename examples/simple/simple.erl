-module(simple).
-export([main/0]).

main() ->
    self() ! ciao,
    receive
        ciao -> done;
        peso -> done
    end.