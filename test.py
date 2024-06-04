#!/bin/python
import os
# import time

test_list = [
    ("./examples/hello/hello.erl", "greet/0", "examples/hello"),
    ("./examples/async/simple.erl","main/0","examples/async"),
    ("./examples/ticktackstop/tictacstop.erl","start/0","examples/ticktackstop"),
    ("./examples/ticktackloop/tictacloop.erl","start/0","examples/ticktackloop"),
    ("./examples/customer/customer.erl","main/0","examples/customer"),
    ("./examples/serverclient/serverclient.erl","main/0","examples/serverclient"),
    ("./examples/trick/trick.erl","main/0","examples/trick"),
    ("./examples/airline/airline.erl","main/0","examples/airline"),
    ("./examples/conditional-case/conditional_case.erl","main/0","examples/conditional-case"),
    ("./examples/for-loop-recursion/forloop.erl","main/0","examples/for-loop-recursion"),
    ("./examples/function-call/funny.erl","main/0","examples/function-call"),
    ("./examples/high-order-fun/hof.erl","greet/0","examples/high-order-fun"),
    ("./examples/if-cases/ifcases.erl","main/0","examples/if-cases"),
    ("./examples/test/foo1/foo1.erl","test/0","examples/test/foo1"),
    ("./examples/test/foo2/foo2.erl","test/0","examples/test/foo2"),
    ("./examples/test/foo3/foo3.erl","test/0","examples/test/foo3"),
    ("./examples/test/foo4/foo4.erl","test/0","examples/test/foo4"),
    ("./examples/test/foo5/foo5.erl","test/0","examples/test/foo5"),
    ("./examples/test/foo6/foo6.erl","test/0","examples/test/foo6"),
    ("./examples/test/ping/ping.erl","start/0","examples/test/ping"),
    ("./examples/cauder_suite/airline/airline.erl","main/0","examples/cauder_suite/airline"),
    # ("./examples/cauder_suite/barber/barber.erl","main/0","examples/cauder_suite/barber"),
    ("./examples/cauder_suite/meViolation/meViolation.erl","main/0","examples/cauder_suite/meViolation"),
    ("./examples/cauder_suite/purchase/purchase.erl","main/0","examples/cauder_suite/purchase"),
]

os.system("rebar3 escriptize")

for item in test_list:
    path, entrypoint, dir = item
    cmd = f"./_build/default/bin/chorer {path} {entrypoint} {dir}"
    print(cmd)
    os.system(cmd)
    # time.sleep(1)
    print("\n")
