# ChorEr

ChorEr is a static analyzer to generate Choreography Automata from Erlang source code.

## Requirements

The project requires `erlang` and `rebar3` to run.

## Usage

Use `rebar3` to run the program from the command line or to compile it.

```erlang
# rebar3 escript
# ./_build/default/bin/chorer ./path/to/your/program.erl main
```

otherwise

```erlang
# rebar3 shell
1> chorer:generate("./examples/ticktack/tictacstop.erl", start).
finished
```

### Output

The tool will create a DOT file for each actor's local view and a DOT file for the global view in the specified folder. To visualize the Choreography Automatas, copy paste the content's files in a [DOT viewer](https://dreampuf.github.io/GraphvizOnline).

## Credits

This project was made for the Bachelor's degree Thesis of the Computer Science course at Alma Mater Studiotum - University of Bologna.
I am grateful to the professor [Ivan Lanese](https://github.com/lanese), who supervised the develpment of this project.
