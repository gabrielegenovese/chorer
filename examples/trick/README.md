# Tick tack stop example

## Use

```bash
./_build/default/bin/chorer ./examples/trick/trick.erl main/0 examples/trick
./_build/default/bin/chorer ./examples/trick/trick2.erl main/0 examples/trick/trick2
```

## Description

Features:

- Numerous registers
- Asynchronous message exchanges
- Functional matching, causing some messages to become dangling.

## Results

Correct LV and GV.

## Other

In `trick2`, adding `_` on lines 15 and 18 can be seen to add transitions in the GV.