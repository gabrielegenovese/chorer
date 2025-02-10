#!/bin/python
import os
import subprocess as sp
import time
import csv
import pydot
import networkx as nx


def load_graph_from_dot(file_path):
    graph = pydot.graph_from_dot_file(file_path)[0]
    G = nx.DiGraph()

    for node in graph.get_nodes():
        node_id = node.get_name()
        label = node.get_label()
        G.add_node(node_id, label=label)

    for edge in graph.get_edges():
        source = edge.get_source()
        destination = edge.get_destination()
        label = edge.get_label()
        G.add_edge(source, destination, label=label)

    return G


def are_graphs_equivalent(G1, G2):
    # check that the nodes are the same
    if set(G1.nodes) != set(G2.nodes):
        return False

    # check that the edges are the same
    if set(G1.edges) != set(G2.edges):
        return False

    # for each nodes in the first graph
    for node in G1.nodes:
        # if the labels are different then they are not equal
        if G1.nodes[node].get("label") != G2.nodes[node].get("label"):
            return False

    # for each edges in the first graph
    for edge in G1.edges:
        # if the labels are different then they are not equal
        if G1[edge[0]][edge[1]].get("label") != G2[edge[0]][edge[1]].get("label"):
            return False

    return True


def check_correctness(file1, file2):
    G1 = load_graph_from_dot(file1)
    try:
        G2 = load_graph_from_dot(file2)
        return str(are_graphs_equivalent(G1, G2))
    except Exception as e:
        return "Not present"


def read_csv_file(filepath):
    data = {}
    with open(filepath, mode="r") as csvfile:
        reader = csv.reader(csvfile)
        for row in reader:
            key, value = row
            data[key] = value
    return data


def lv_number(csvdata):
    c = 0
    for k in csvdata.keys():
        if "lv" in k:
            c += 1
    return str(int(c / 2))  # every lv is double counted


def generate_latex_table(columns, rows, caption, label):
    headers = " & ".join(columns)
    table = (
        "\\begin{table}[!ht]\n\\centering\n\\begin{tabular}{|"
        + "c|" * len(columns)
        + "}\n\\hline\n"
    )
    table += headers + " \\\\ \n\\hline\n"
    for row in rows:
        # print(row)
        table += " & ".join(row) + " \\\\ \n"
    table += (
        "\\hline\n\\end{tabular}\n\\caption{"
        + caption
        + "}\n\\label{"
        + label
        + "}\n\\end{table}"
    )
    return table


# default: minimize global views
test_list = [
    ["./examples/account/account.erl", "main/0", "examples/account", "true"],
    ["./examples/dining/dining.erl", "main/0", "examples/dining", "true"],
    ["./examples/hello/hello.erl", "main/0", "examples/hello", "true"],
    ["./examples/async/simple.erl", "main/0", "examples/async", "true"],
    ["./examples/ticktackstop/tictacstop.erl","start/0","examples/ticktackstop", "true"],
    ["./examples/ticktackloop/tictacloop.erl","start/0","examples/ticktackloop", "true"],
    ["./examples/customer/customer.erl","main/0","examples/customer", "true"],
    ["./examples/serverclient/serverclient.erl","main/0","examples/serverclient", "true"],
    ["./examples/trick/trick.erl","main/0","examples/trick", "true"],
    ["./examples/airline/airline.erl","main/0","examples/airline", "true"],
    ["./examples/conditional/conditional.erl","main/0","examples/conditional", "true"],
    ["./examples/forloop/forloop.erl","main/0","examples/forloop", "true"],
    ["./examples/funcall/funcall.erl","main/0","examples/funcall", "true"],
    ["./examples/hof/hof.erl","greet/0","examples/hof", "true"],
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

# compile the tool
os.system("rebar3 escriptize")

csvs = []
add_data = {}

# run the tool on each test
for item in test_list:
    item = ["./_build/default/bin/chorer"] + item[:-1]  # remove minimize of global view
    csvfile = item[3] + "/output.csv"
    gvfile = item[3] + "/global_view.dot"
    correct_gvfile = item[3] + "/correct_gv.dot"
    csvs.append(csvfile)
    print("Executing", " ".join(item))
    # get time
    start_time = time.time()
    output = sp.check_output(item).decode("utf-8")
    runtime = time.time() - start_time
    # get some additional data
    warns = output.count("WARNING")
    errs = output.count("ERROR")
    add_data[item[3].rsplit("/", 1)[-1]] = {
        "warns": str(warns),
        "errs": str(errs),
        "runtime": "{:.3f}s".format(runtime),
        "correct": check_correctness(gvfile, correct_gvfile),
    }
    # print(f"{output}\ntime {runtime}\nwarns {warns}\nerrs {errs}")
    # time.sleep(1)


# collect data from csv
datas = []
for c in csvs:
    data = read_csv_file(c)
    filetmp = c.rsplit("/", 1)[0] if "/" in c else c
    file = filetmp.rsplit("/", 1)[-1] if "/" in c else c
    # merge the 2 dictionaries with data
    datas.append((file, data | add_data[file]))

# sort by number of lines
sorted_data = sorted(datas, key=lambda x: x[1]["line"])

# generate global view table
columns = [
    "Example",
    "Lines",
    "Tot LV",
    "GV Nodes",
    "GV Edges",
    "Warns",
    "Errors",
    "Time",
    # "Check",
]
rows = [
    [
        file,
        data["line"],
        lv_number(data),
        data["gv_nodes"],
        data["gv_edges"],
        data["warns"],
        data["errs"],
        data["runtime"],
        # data["correct"],
    ]
    for (file, data) in sorted_data
]

gv_table_code = generate_latex_table(
    columns, rows, "Global view empirical data", "tab:gvbench"
)

with open("assets/table.tex", "w", encoding="utf-8") as f:
    f.write(gv_table_code)

# generate correctness view table
columns = [
    "Example",
    "Check",
]
rows = [
    [
        file,
        data["correct"],
    ]
    for (file, data) in sorted_data
    if "Not present" != data["correct"]
]

correct_table_code = generate_latex_table(
    columns, rows, "Global view correctness data", "tab:corrbench"
)

with open("assets/correct.tex", "w", encoding="utf-8") as f:
    f.write(correct_table_code)
