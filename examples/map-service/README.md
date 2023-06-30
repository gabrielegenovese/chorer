# Usage

```erlang
# erl
1> c(server).
{ok,server}
2> c(client).
{ok,client}
3> Server = server:start().
<0.94.0>
4> client:get_elem(Server, 2).
error
5> client:set_elem(Server, 1, "hello").
{ok,1,"hello"}
6> client:set_elem(Server, 2, "mark").
{ok,2,"mark"}
7> client:get_elem(Server, 2).
{ok,"mark"}
8> client:get_size(Server).
{ok,2}
9> client:get_key_list(Server).
{ok,[1,2]}
10> client:del_elem(Server, 2).
{ok,"mark"}
11> client:get_elem(Server, 2).
error
12> client:elem_exist(Server, 3).
error
13> client:elem_exist(Server, 1). % works like get_elem
{ok,"hello"}
```
