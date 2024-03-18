# Airline example

## Usage

```bash
./_build/default/bin/chorer ./examples/airline/airline.erl main/0 ./examples/airline
```

## Description

Features:

- `seat` is a function with two definitions, and thanks to the `eval:function_list` function, this case is handled correctly by restarting the graph construction from the first vertex.
- In the main function, the `seat` function is called and correctly attached to the main function using the `merge_graph` function (variable passing is not evaluated).
- During the construction of the GV (Global View), variable passing of spawn and send/recv is correctly evaluated.

## Results

The GV does not seem correct; the issue appears to be that it returns to previous states when it should not.
The LV (Local View) of the main function is very generic; it could be more precise if the evaluation of variable 2 is performed (a correct LV is attached in the folder).