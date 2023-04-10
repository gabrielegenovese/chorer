# Usage

In a ternimal:

```erlang
# erl
1> c(server).
{ok,server}
2> server:start(8080, 8081).
Message:
```

In a second ternimal:

```erlang
# erl
1> c(server).
{ok,server}
2> server:start(8081, 8080).
Message:
```

Start messaging. Send an empty message to close the application.
Credits to [zorched.net](https://www.zorched.net/2008/05/29/erlang-examples-talk-with-sockets/)
