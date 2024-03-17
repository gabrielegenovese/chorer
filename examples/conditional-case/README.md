# Conditional case example

## Use

```bash
./_build/default/bin/chorer ./examples/conditional-case/conditional_case.erl main0 ./examples/conditional-case
```

## Description

Features:

- vengono spawnati diversi processi dalla stessa funzione a cui viene passato il pid del main
- in questo esempio vengono usate le variabili per eseguire del pattern matching a cui poi corrisponde una send precisa

## Results

La GV cambia molto a seconda delle recv perché non vengono gestite bene le stringhe e le variabili con il pm

provare ad aggiungere alla fine del main

```erlang
D -> D ! "Ciao D";
        true -> io:fwrite("Boh~n")
    end,

    receive
        Obj -> Obj
    end.
```
