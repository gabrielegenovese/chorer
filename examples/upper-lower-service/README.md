# Usage

```erlang
# erl
1> c(server).
{ok,server}
2> c(client).
{ok,client}
3> Server = server:start().
<0.94.0>
4> client:send_string(Server, "hello world", uppercase).
Sending hello world
Server returned HELLO WORLD
ok
5> client:send_string(Server, "HELLO WORLD", lowercase).
Sending HELLO WORLD
Server returned hello world
ok
```