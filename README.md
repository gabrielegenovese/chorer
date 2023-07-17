# ChorEr

ChorEr is a static analyzer to generate Choreography Automata from Erlang source code.

## Requirements

The project requires `erlang` and `rebar3` to run.

## Usage

Use `rebar3` to run the program from the Command Line.

```erlang
# rebar3 shell
1> chorer_app:generate("./examples/ticktack/tictacstop.erl", start).
finished
```

The tool will create a DOT file for each actor's local view and a DOT for the global view in your folder. To visualize the Choreography Automatas, copy paste the content's files in a [DOT viewer](https://dreampuf.github.io/GraphvizOnline).


## Credits

This project was made for the Bachelor's degree Thesis of the Computer Science course at Alma Mater Studiotum - University of Bologna.
I am grateful to the professor [Ivan Lanese](https://github.com/lanese), who supervised the develpment of this project.