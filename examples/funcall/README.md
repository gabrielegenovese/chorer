# Function call example

## Use

```bash
./_build/default/bin/chorer ./examples/function-call/funny.erl main/0 examples/function-call
```

## Description

Features:

- Two identical calls to `recv_dummy`.

## Results

Incorrect LV of the main function because during minimization, it removes the second receive, resulting in an incorrect GV.