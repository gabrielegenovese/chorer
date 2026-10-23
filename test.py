#!/usr/bin/env python3
"""Run chorer on every example and write the results as CSV.

Output: OUT_DIR/results.csv, one row per example (metrics + correctness status).
The details of every correctness failure are printed on the console.

Exit code is 1 if any run failed or any correctness check is "fail"/"error".
"""
import csv
import os
import re
import subprocess as sp
import sys
import time
from collections import Counter

import networkx as nx
import pydot

CHORER = "./_build/default/bin/chorer"
OUT_DIR = "assets"
TIMEOUT = 600  # seconds, per example

# [erl file, entry point, output dir, (unused) minimize flag]
TEST_LIST = [
    ["./examples/account/account.erl", "main/0", "examples/account", "true"],
    ["./examples/dining/dining.erl", "main/0", "examples/dining", "true"],
    ["./examples/hello/hello.erl", "main/0", "examples/hello", "true"],
    ["./examples/async/async.erl", "main/0", "examples/async", "true"],
    ["./examples/ticktackstop/tictacstop.erl", "start/0", "examples/ticktackstop", "true"],
    ["./examples/ticktackloop/tictacloop.erl", "start/0", "examples/ticktackloop", "true"],
    ["./examples/customer/customer.erl", "main/0", "examples/customer", "true"],
    ["./examples/serverclient/serverclient.erl", "main/0", "examples/serverclient", "true"],
    ["./examples/trick/trick.erl", "main/0", "examples/trick", "true"],
    ["./examples/airline/airline.erl", "main/0", "examples/airline", "true"],
    ["./examples/conditional/conditional.erl", "main/0", "examples/conditional", "true"],
    ["./examples/forloop/forloop.erl", "main/0", "examples/forloop", "true"],
    ["./examples/funcall/funcall.erl", "main/0", "examples/funcall", "true"],
    ["./examples/hof/hof.erl", "greet/0", "examples/hof", "true"],
    ["./examples/if-cases/ifcases.erl", "main/0", "examples/if-cases", "true"],
    ["./examples/pass/pass.erl", "main/0", "examples/pass", "true"],
    ["./examples/producer/producer.erl", "main/0", "examples/producer", "true"],
    ["./examples/spawn/spawn.erl", "main/0", "examples/spawn", "true"],
    ["./examples/unknown/unknown.erl", "main/0", "examples/unknown", "true"],
    ["./examples/test/foo1/foo1.erl", "test/0", "examples/test/foo1", "true"],
    ["./examples/test/foo2/foo2.erl", "test/0", "examples/test/foo2", "true"],
    ["./examples/test/foo3/foo3.erl", "test/0", "examples/test/foo3", "true"],
    ["./examples/test/foo4/foo4.erl", "test/0", "examples/test/foo4", "true"],
    ["./examples/test/foo5/foo5.erl", "test/0", "examples/test/foo5", "true"],
    ["./examples/test/foo6/foo6.erl", "test/0", "examples/test/foo6", "true"],
    ["./examples/test/foo7/foo7.erl", "main/0", "examples/test/foo7", "true"],
    ["./examples/test/foo8/foo8.erl", "main/0", "examples/test/foo8", "true"],
    ["./examples/test/foo9/foo9.erl", "main/0", "examples/test/foo9", "true"],
    ["./examples/test/foo9b/foo9b.erl", "main/0", "examples/test/foo9b", "true"],
    ["./examples/test/foo9c/foo9c.erl", "main/0", "examples/test/foo9c", "true"],
    ["./examples/test/foo9d/foo9d.erl", "main/0", "examples/test/foo9d", "true"],
    ["./examples/test/foo9e/foo9e.erl", "main/0", "examples/test/foo9e", "true"],
    ["./examples/test/foo9f/foo9f.erl", "main/0", "examples/test/foo9f", "true"],
    ["./examples/test/foo9g/foo9g.erl", "main/0", "examples/test/foo9g", "true"],
    ["./examples/test/foo9h/foo9h.erl", "main/0", "examples/test/foo9h", "true"],
    ["./examples/test/ping/ping.erl", "start/0", "examples/test/ping", "true"],
    ["./examples/cauder_suite/airline/airline.erl", "main/0", "examples/cauder_suite/airline", "true"],
    ["./examples/cauder_suite/meViolation/meViolation.erl", "main/0", "examples/cauder_suite/meViolation", "true"],
    ["./examples/cauder_suite/purchase/purchase.erl", "main/0", "examples/cauder_suite/purchase", "true"],
    # ["./examples/cauder_suite/barber/barber.erl", "main/0", "examples/cauder_suite/barber", "true"],
]

FIELDS = [
    "example", "lines", "total_lv", "gv_nodes", "gv_edges",
    "warnings", "errors", "runtime_s", "exit_code", "correct",
]


# --------------------------------------------------------------------------
# Correctness check: is the generated global view the same automaton as the
# reference one?
# --------------------------------------------------------------------------

def clean(value):
    """Remove DOT quoting and surrounding whitespace ('"1"' and 1 are the same)."""
    return None if value is None else value.strip().strip('"').strip()


def _collect(dot_graph, G):
    for node in dot_graph.get_nodes():
        name = clean(node.get_name())
        # "node [...]", "edge [...]", "graph [...]" are default-attribute
        # statements, not real nodes
        if not name or name in ("node", "edge", "graph"):
            continue
        G.add_node(name, label=clean(node.get("label")), shape=clean(node.get("shape")))
    for edge in dot_graph.get_edges():
        G.add_edge(clean(edge.get_source()), clean(edge.get_destination()),
                   label=clean(edge.get("label")))
    for sub in dot_graph.get_subgraphs():
        _collect(sub, G)


def load_graph_from_dot(path):
    graphs = pydot.graph_from_dot_file(path)
    if isinstance(graphs, list):
        graphs = graphs[0] if graphs else None
    if graphs is None:
        raise ValueError("cannot parse " + path)
    # MultiDiGraph: two edges between the same states with different labels
    # (very common in a choreography) must not overwrite each other
    G = nx.MultiDiGraph()
    _collect(graphs, G)
    return G


def node_signature(attrs):
    """What identifies a state apart from its name.

    State names are numbers assigned by the renaming BFS, so they are not
    comparable between two runs. We keep only the part of the label that is
    not a number (the "final" tag) and the shape.
    ASSUMPTION: adapt this if your DOT files carry other information on nodes.
    """
    return (re.sub(r"\d+", "", attrs.get("label") or ""), attrs.get("shape"))


def edge_labels(parallel_edges):
    """Sorted labels of all the edges between the same two states."""
    return sorted(str(d.get("label")) for d in parallel_edges.values())


def graphs_equivalent(G1, G2):
    """Return (True/False, reason). Equivalent = isomorphic, ignoring state names."""
    if G1.number_of_nodes() != G2.number_of_nodes():
        return False, f"nodes: {G1.number_of_nodes()} vs {G2.number_of_nodes()}"
    if G1.number_of_edges() != G2.number_of_edges():
        return False, f"edges: {G1.number_of_edges()} vs {G2.number_of_edges()}"
    l1 = Counter(str(d.get("label")) for _, _, d in G1.edges(data=True))
    l2 = Counter(str(d.get("label")) for _, _, d in G2.edges(data=True))
    if l1 != l2:
        diff = sorted(((l1 - l2) + (l2 - l1)).keys())
        return False, f"different edge labels: {diff}"
    s1 = Counter(node_signature(d) for _, d in G1.nodes(data=True))
    s2 = Counter(node_signature(d) for _, d in G2.nodes(data=True))
    if s1 != s2:
        return False, f"different node labels/shapes: {dict(s1)} vs {dict(s2)}"
    iso = nx.is_isomorphic(
        G1, G2,
        node_match=lambda a, b: node_signature(a) == node_signature(b),
        edge_match=lambda a, b: edge_labels(a) == edge_labels(b),
    )
    return (True, "") if iso else (False, "same labels but different structure")


def check_correctness(gv_file, reference_file):
    """Return (status, detail); status is pass / fail / error / no_reference."""
    if not os.path.exists(reference_file):
        return "no_reference", ""
    if not os.path.exists(gv_file):
        return "error", "global view not generated"
    try:
        ok, why = graphs_equivalent(load_graph_from_dot(gv_file),
                                    load_graph_from_dot(reference_file))
    except Exception as e:  # unreadable/invalid DOT: report it, do not hide it
        return "error", f"{type(e).__name__}: {e}"
    return ("pass", "") if ok else ("fail", why)


# --------------------------------------------------------------------------
# Running the tool
# --------------------------------------------------------------------------

def read_csv_file(path):
    data = {}
    if not os.path.exists(path):
        return data
    with open(path, newline="") as f:
        for row in csv.reader(f):
            if len(row) == 2:
                data[row[0]] = row[1]
    return data


def lv_number(data):
    # every lv appears twice in the csv
    return sum("lv" in k for k in data) // 2


def to_int(value):
    try:
        return int(value)
    except (TypeError, ValueError):
        return None


def run_example(erl_file, entry, out_dir):
    csv_file = os.path.join(out_dir, "output.csv")
    gv_file = os.path.join(out_dir, "global_view.dot")
    reference = os.path.join(out_dir, "correct_gv.dot")

    # a leftover from a previous run must not be mistaken for a fresh result
    for stale in (csv_file, gv_file):
        if os.path.exists(stale):
            os.remove(stale)

    cmd = [CHORER, erl_file, entry, out_dir]
    print("Executing", " ".join(cmd), flush=True)
    start = time.time()
    try:
        proc = sp.run(cmd, capture_output=True, text=True, errors="replace", timeout=TIMEOUT)
        exit_code, stdout, stderr = proc.returncode, proc.stdout, proc.stderr
    except sp.TimeoutExpired:
        exit_code, stdout, stderr = "timeout", "", ""
    runtime = time.time() - start
    if exit_code != 0:
        print(f"  !! chorer failed ({exit_code})\n{stderr[-500:]}", file=sys.stderr)

    metrics = read_csv_file(csv_file)
    status, detail = check_correctness(gv_file, reference)
    name = out_dir.removeprefix("examples/")  # unique, unlike the last path component
    return {
        "example": name,
        "lines": to_int(metrics.get("line")),
        "total_lv": lv_number(metrics) if metrics else None,
        "gv_nodes": to_int(metrics.get("gv_nodes")),
        "gv_edges": to_int(metrics.get("gv_edges")),
        "warnings": stdout.count("WARNING"),
        "errors": stdout.count("ERROR"),
        "runtime_s": round(runtime, 3),
        "exit_code": exit_code,
        "correct": status,
        "detail": detail,  # printed on the console, not written to the csv
    }


def main(build=True):
    if build and os.system("rebar3 escriptize") != 0:
        sys.exit("rebar3 escriptize failed")

    results = [run_example(erl, entry, out_dir) for erl, entry, out_dir, _ in TEST_LIST]
    # numeric sort (the original sorted the strings: "100" < "20")
    results.sort(key=lambda r: (r["lines"] is None, r["lines"] or 0))

    os.makedirs(OUT_DIR, exist_ok=True)
    with open(os.path.join(OUT_DIR, "results.csv"), "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=FIELDS, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(results)

    counts = Counter(r["correct"] for r in results)
    crashed = [r["example"] for r in results if r["exit_code"] != 0]
    print("\ncorrectness:", dict(counts))
    for r in results:
        if r["correct"] in ("fail", "error"):
            print(f"  {r['correct']:5s} {r['example']}: {r['detail']}")
    if crashed:
        print("chorer failed on:", ", ".join(crashed))
    return 1 if crashed or counts["fail"] or counts["error"] else 0


if __name__ == "__main__":
    sys.exit(main())