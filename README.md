# ChorEr

ChorEr is a static analyzer to generate Choreography Automata from Erlang source code.

## Requirements

The project requires `erlang` and `rebar3` to run.

## Usage

Use `rebar3` to run the program from the command line or to compile it.

```erlang
# rebar3 escript
# ./_build/default/bin/chorer ./path/to/your/program.erl main path/to/folder
```

or

```erlang
# rebar3 shell
1> chorer:generate("./examples/ticktack/tictacstop.erl", start).
finished
```

### Output

The tool will create a DOT file for each actor's local view and a DOT file for the global view in the specified folder. To visualize the Choreography Automatas copy and paste the `.dot` files' content in a [DOT viewer](https://dreampuf.github.io/GraphvizOnline).

## Documentation

The documentation of the project is aviable at this [link](https://gabrielegenovese.github.io/chorer/). You can also generete it with

```erlang
# rebar3 ex_doc --output docs
```

## Credits

This project was made for the Bachelor's degree Thesis of the Computer Science course at Alma Mater Studiorum - University of Bologna. I am grateful to the professor [Ivan Lanese](http://www.cs.unibo.it/~lanese/), who supervised the development of this project.
