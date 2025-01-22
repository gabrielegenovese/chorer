# Foo6 example

## Use

```bash
./_build/default/bin/chorer ./examples/test/foo6/foo6.erl test/0 examples/test/foo6
```

## Description

Features:

- two definitions for client_gen
- multiple variable argument passing

## Results

| File                    | state                                                   |
| ----------------------- | ------------------------------------------------------- |
| client_1_local_view     | ok                                                      |
| client_gen_2_local_view | in the loop there should be only `S`?                   |
| server_0_local_view     | ok                                                      |
| test_0_local_view       | ok                                                      |
| global view             | random spawn, don't understend how last loop is created |

Debug global