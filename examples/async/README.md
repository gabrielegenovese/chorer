# Async example

## Usage

```bash
./_build/default/bin/chorer ./examples/async/simple.erl main/0 ./examples/async
```

## Description

Features:

- In this simple example, two sends occur simultaneously, so in the global view, it correctly represents that there are two possible executions.

## Results

LV and GV are correct.

<img src='https://g.gravizo.com/svg?
    digraph global {
        rankdir="LR";
        n_0 [label="global", shape="plaintext"];
        n_1 [id="5", shape=circle, label="5"];
        n_2 [id="6", shape=circle, label="6"];
        n_3 [id="1", shape=circle, label="1"];
        n_0 -> n_3 [arrowhead=none];
        n_4 [id="2", shape=circle, label="2"];
        n_5 [id="4", shape=circle, label="4"];
        n_6 [id="7", shape=circle, label="7"];
        n_7 [id="3", shape=circle, label="3"];
        n_7 -> n_5 [id="[$e|2]", label="dummy2/0.0→dummy1/0.0:ciao"];
        n_7 -> n_1 [id="[$e|3]", label="dummy1/0.0→dummy2/0.0:bello"];
        n_1 -> n_6 [id="[$e|5]", label="dummy2/0.0→dummy1/0.0:ciao"];
        n_4 -> n_7 [id="[$e|1]", label="main/0.0Δdummy2/0.0"];
        n_3 -> n_4 [id="[$e|0]", label="main/0.0Δdummy1/0.0"];
        n_5 -> n_2 [id="[$e|4]", label="dummy1/0.0→dummy2/0.0:bello"];
    }
'/>
