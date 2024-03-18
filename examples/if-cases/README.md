# If example

## Use

```bash
./_build/default/bin/chorer ./examples/if-cases/ifcases.erl main/0 examples/if-cases
```

## Description

Features:

- Usage of `if` to diversify computation
- Passing PID between multiple processes

## Results

Correct LV, GV does not produce transitions where the string is passed, probably because the variable is not evaluated properly.