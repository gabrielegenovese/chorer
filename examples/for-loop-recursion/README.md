# For loop receive example

## Usage

```bash
./_build/default/bin/chorer ./examples/for-loop-recursion/forloop.erl main/0 ./examples/for-loop-recursion
```

## Description

Features:

- Receive in a recursive function
- Deadlock occurs because the main process remains waiting for a message, but there are only two processes in the system.

## Results

Correct LV. In GV, there is a need to implement a system to determine if it is in a final state. Then, the deadlock would be notified!
