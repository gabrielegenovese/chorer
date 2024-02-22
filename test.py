#!/bin/python
import os

test_list = [
    ("./examples/hello/hello.erl", "greet0", "examples/hello"),
    ("./examples/async/simple.erl","main0","examples/async"),
    ("./examples/ticktackstop/tictacstop.erl","start0","examples/ticktackstop"),
    ("./examples/ticktackloop/tictacloop.erl","start0","examples/ticktackloop"),
    ("./examples/customer/customer.erl","main0","examples/customer"),
    ("./examples/serverclient/serverclient.erl","main0","examples/serverclient"),
    ("./examples/trick/trick.erl","main0","examples/trick"),
    ("./examples/airline/airline.erl","main0","examples/airline"),
    ("./examples/conditional-case/conditional_case.erl","main0","examples/conditional-case"),
    ("./examples/for-loop-recursion/forloop.erl","main0","examples/for-loop-recursion"),
    ("./examples/function-call/funny.erl","main0","examples/function-call"),
    ("./examples/high-order-fun/hof.erl","greet0","examples/high-order-fun"),
    ("./examples/if-cases/ifcases.erl","main0","examples/if-cases"),
    ("./examples/violation/meViolation.erl", "", "examples/violation"),
    ("./examples/test/barber/barber.erl","main0","examples/test/barber"),
    ("./examples/test/foo1/foo1.erl","test0","examples/test/foo1"),
    ("./examples/test/foo2/foo2.erl","test0","examples/test/foo2"),
    ("./examples/test/foo3/foo3.erl","test0","examples/test/foo3"),
    ("./examples/test/foo4/foo4.erl","test0","examples/test/foo4"),
    ("./examples/test/foo5/foo5.erl","test0","examples/test/foo5"),
    ("./examples/test/foo6/foo6.erl","test0","examples/test/foo6"),
    ("./examples/test/ping/ping.erl","start0","examples/test/ping"),
]

os.system("rebar3 escriptize")

for item in test_list:
    path, entrypoint, dir = item
    cmd = f"./_build/default/bin/chorer {path} {entrypoint} {dir}"
    print(cmd)
    os.system(cmd)
    print("\n")
