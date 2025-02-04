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
    ["./examples/pass/pass.erl","main/0","examples/pass", "true"],
    ["./examples/producer/producer.erl","main/0","examples/producer", "true"],
    ["./examples/spawn/spawn.erl","main/0","examples/spawn", "true"],
    ["./examples/unknown/unknown.erl","main/0","examples/unknown", "true"],
    ["./examples/test/foo1/foo1.erl","test/0","examples/test/foo1", "true"],
    ["./examples/test/foo2/foo2.erl","test/0","examples/test/foo2", "true"],
    ["./examples/test/foo3/foo3.erl","test/0","examples/test/foo3", "true"],
    ["./examples/test/foo4/foo4.erl","test/0","examples/test/foo4", "true"],
    ["./examples/test/foo5/foo5.erl","test/0","examples/test/foo5", "true"],
    ["./examples/test/foo6/foo6.erl","test/0","examples/test/foo6", "true"],
    ["./examples/test/foo7/foo7.erl","main/0","examples/test/foo7", "true"],
    ["./examples/test/foo8/foo8.erl","main/0","examples/test/foo8", "true"],
    ["./examples/test/foo9/foo9.erl","main/0","examples/test/foo9", "true"],
    ["./examples/test/foo9b/foo9b.erl","main/0","examples/test/foo9b", "true"],
    ["./examples/test/foo9c/foo9c.erl","main/0","examples/test/foo9c", "true"],
    ["./examples/test/foo9d/foo9d.erl","main/0","examples/test/foo9d", "true"],
    ["./examples/test/foo9e/foo9e.erl","main/0","examples/test/foo9e", "true"],
    ["./examples/test/foo9f/foo9f.erl","main/0","examples/test/foo9f", "true"],
    ["./examples/test/foo9g/foo9g.erl","main/0","examples/test/foo9g", "true"],
    ["./examples/test/foo9h/foo9h.erl","main/0","examples/test/foo9h", "true"],
    ["./examples/test/ping/ping.erl","start/0","examples/test/ping", "true"],
    ["./examples/cauder_suite/airline/airline.erl","main/0","examples/cauder_suite/airline", "true"],
    ["./examples/cauder_suite/meViolation/meViolation.erl","main/0","examples/cauder_suite/meViolation", "true"],
    ["./examples/cauder_suite/purchase/purchase.erl","main/0","examples/cauder_suite/purchase", "true"],
    # ["./examples/cauder_suite/barber/barber.erl","main/0","examples/cauder_suite/barber", "true"],
]

os.system("rebar3 escriptize")

outputs = []

for item in test_list:
    item = ["./_build/default/bin/chorer"] + item[:-1] # remove minimize of global view
    print("Executing ", " ".join(item))
    start_time = time.time()
    output = subprocess.check_output(item).decode('utf-8')
    runtime = time.time() - start_time
    print(f"{output}\ntime {runtime}\n")
    outputs.append(output)
    # time.sleep(1)

