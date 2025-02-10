# Conditional case example

## Usage

```bash
./_build/default/bin/chorer ./examples/conditional-case/conditional_case.erl main/0 ./examples/conditional-case
```

## Description

Features:

- Several processes are spawned from the same function, to which the main process's PID is passed.
- Variables are used in this example to perform pattern matching, followed by a precise send operation.

## Results

Strings are not handled correctly. The GV changes significantly depending on the receives because strings and variables with pattern matching are not handled well.

Try adding the following to the end of the main function:

```erlang
...
        D -> D ! "Ciao D";
        true -> io:fwrite("Boh~n")
    end,

    receive
        Obj -> Obj
    end.
```
