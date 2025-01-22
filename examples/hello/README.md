# Hello example

## Use

```bash
./_build/default/bin/chorer ./examples/hello/hello.erl main/0 examples/hello
```

## Description

Features:

- Usage of `self` to send messages to itself
- Simulation of nondeterministic `if` where in one branch it concludes and in the other, it goes into recursion.

## Results

Correct LV, incorrect GV because in state 2, it does not execute the second send and directly goes to the recv. There is no implementation for the branching when there is both a recv and another transition in one state simultaneously.
