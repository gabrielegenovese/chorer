#!/bin/python
import os
import subprocess
import time

# default: minimize global views
test_list = [
    ["./examples/account/account.erl", "main/0", "examples/account", "true"],
    ["./examples/dining/dining.erl", "main/0", "examples/dining", "true"],
    ["./examples/hello/hello.erl", "main/0", "examples/hello", "true"],
    ["./examples/async/simple.erl","main/0","examples/async", "true"],
    ["./examples/ticktackstop/tictacstop.erl","start/0","examples/ticktackstop", "true"],
    ["./examples/ticktackloop/tictacloop.erl","start/0","examples/ticktackloop", "true"],
    ["./examples/customer/customer.erl","main/0","examples/customer", "true"],
    ["./examples/serverclient/serverclient.erl","main/0","examples/serverclient", "true"],
    ["./examples/trick/trick.erl","main/0","examples/trick", "true"],
    ["./examples/airline/airline.erl","main/0","examples/airline", "true"],
    ["./examples/conditional-case/conditional_case.erl","main/0","examples/conditional-case", "true"],
    ["./examples/for-loop-recursion/forloop.erl","main/0","examples/for-loop-recursion", "true"],
    ["./examples/function-call/funny.erl","main/0","examples/function-call", "true"],
    ["./examples/high-order-fun/hof.erl","greet/0","examples/high-order-fun", "true"],
    ["./examples/if-cases/ifcases.erl","main/0","examples/if-cases", "true"],
    ["./examples/test/foo1/foo1.erl","test/0","examples/test/foo1", "true"],
    ["./examples/test/foo2/foo2.erl","test/0","examples/test/foo2", "true"],
    ["./examples/test/foo3/foo3.erl","test/0","examples/test/foo3", "true"],
    ["./examples/test/foo4/foo4.erl","test/0","examples/test/foo4", "true"],
    ["./examples/test/foo5/foo5.erl","test/0","examples/test/foo5", "true"],
    ["./examples/test/foo6/foo6.erl","test/0","examples/test/foo6", "true"],
    ["./examples/test/ping/ping.erl","start/0","examples/test/ping", "true"],
    ["./examples/cauder_suite/airline/airline.erl","main/0","examples/cauder_suite/airline", "true"],
    # ["./examples/cauder_suite/barber/barber.erl","main/0","examples/cauder_suite/barber", "true"],
    ["./examples/cauder_suite/meViolation/meViolation.erl","main/0","examples/cauder_suite/meViolation", "true"],
    ["./examples/cauder_suite/purchase/purchase.erl","main/0","examples/cauder_suite/purchase", "true"],
]

os.system("rebar3 escriptize")

outputs = []

for item in test_list:
    item = ["./_build/default/bin/chorer"] + item
    print("Executing ", " ".join(item))
    output = subprocess.check_output(item).decode('utf-8')
    print(output + "\n")
    outputs.append(output)
    time.sleep(1)

