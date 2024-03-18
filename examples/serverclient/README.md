# Sever Client example

## Use

```bash
./_build/default/bin/chorer ./examples/serverclient/serverclient.erl main/0 examples/serverclient
```

## Description

Features:

- Spawning in a loop
- Passing PIDs and messages

## Results

Correct LV and GV with only one client.

## Other

If another client is added in the main function, the result of the GV becomes interesting but incorrect.

This is a good example to understand the problem of generalizing spawns, as addressed in the reference paper. What should be written in the LV if I execute the spawn of the client twice? In the LV of the client, when I do `self()`, I should return `client/0.N` instead of just `0`, so that in the GV, it accurately reflects the current one.
