#!/bin/python
import os
# import time

test_list = [
    ("./examples/account/account.erl", "main/0", "examples/account", "true", "true"),
    ("./examples/dining/dining.erl", "main/0", "examples/dining", "true", "true"),
    ("./examples/hello/hello.erl", "main/0", "examples/hello", "true", "true"),
    ("./examples/async/simple.erl","main/0","examples/async", "true", "true"),
    ("./examples/ticktackstop/tictacstop.erl","start/0","examples/ticktackstop", "true", "true"),
    ("./examples/ticktackloop/tictacloop.erl","start/0","examples/ticktackloop", "true", "true"),
    ("./examples/customer/customer.erl","main/0","examples/customer", "true", "true"),
    ("./examples/serverclient/serverclient.erl","main/0","examples/serverclient", "true", "true"),
    ("./examples/trick/trick.erl","main/0","examples/trick", "true", "true"),
    ("./examples/airline/airline.erl","main/0","examples/airline", "true", "true"),
    ("./examples/conditional-case/conditional_case.erl","main/0","examples/conditional-case", "true", "true"),
    ("./examples/for-loop-recursion/forloop.erl","main/0","examples/for-loop-recursion", "true", "true"),
    ("./examples/function-call/funny.erl","main/0","examples/function-call", "true", "true"),
    ("./examples/high-order-fun/hof.erl","greet/0","examples/high-order-fun", "true", "true"),
    ("./examples/if-cases/ifcases.erl","main/0","examples/if-cases", "true", "true"),
    ("./examples/test/foo1/foo1.erl","test/0","examples/test/foo1", "true", "true"),
    ("./examples/test/foo2/foo2.erl","test/0","examples/test/foo2", "true", "true"),
    ("./examples/test/foo3/foo3.erl","test/0","examples/test/foo3", "true", "true"),
    ("./examples/test/foo4/foo4.erl","test/0","examples/test/foo4", "true", "true"),
    ("./examples/test/foo5/foo5.erl","test/0","examples/test/foo5", "true", "true"),
    ("./examples/test/foo6/foo6.erl","test/0","examples/test/foo6", "true", "true"),
    ("./examples/test/ping/ping.erl","start/0","examples/test/ping", "true", "true"),
    ("./examples/cauder_suite/airline/airline.erl","main/0","examples/cauder_suite/airline", "true", "true"),
    # ("./examples/cauder_suite/barber/barber.erl","main/0","examples/cauder_suite/barber", "true", "true"),
    ("./examples/cauder_suite/meViolation/meViolation.erl","main/0","examples/cauder_suite/meViolation", "true", "true"),
    ("./examples/cauder_suite/purchase/purchase.erl","main/0","examples/cauder_suite/purchase", "true", "true"),
]

os.system("rebar3 escriptize")

for item in test_list:
    path, entrypoint, dir, minl, ming = item
    cmd = f"./_build/default/bin/chorer {path} {entrypoint} {dir} {minl} {ming}"
    print(cmd)
    os.system(cmd)
    # time.sleep(1)
    print("\n")
