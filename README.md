# ChorEr

ChorEr is a static analyzer to generate Choreography Automata from Erlang source code.
A web version is also [available](https://chorer.cappuccino.ovh/).

## Requirements

The project requires `erlang` and `rebar3` to run.

## Usage

Use `rebar3` to compile it and run the binary:

```
shell> rebar3 escriptize
shell> ./_build/default/bin/chorer
Usage:
  chorer <input> <entrypoint> <output> <minl> <ming>

Extract a choreography automata of an Erlang program.

Arguments:
  input      Erlang soure file (string)
  entrypoint Entrypoint of the program (atom)
  output     Output directory for the generated dot files (string), default: .
  minl       Minimize the localviews , default: true
  ming       Minimize the globalviews , default: false
```

Otherwise, use it with the Erlang shell:

```
shell> rebar3 shell
1> chorer:generate("./examples/ticktack/tictacstop.erl", start/0).
...
[INFO] Finished!
```

### Output

The tool will create a DOT file for each actor's local view and a DOT file for the global view in the specified folder. To visualize the Choreography Automata copy and paste the `.dot` files' content in a [DOT viewer](https://dreampuf.github.io/GraphvizOnline).

## Documentation

The documentation of the project is available at this [link](https://gabrielegenovese.github.io/chorer/). You can also generate it with

```
shell> rebar3 ex_doc --output docs
```

## Test

Try out the tool using the `./test.py` script.

## Supported expression

| Expression     | Support        |
| -------------- | -------------- |
| atom           | ✅ yes         |
| integer        | ✅ yes         |
| float          | ✅ yes         |
| boolean        | ✅ yes         |
| tuple          | ✅ yes         |
| list           | 🟡 partial     |
| string         | 🟡 partial     |
| record         | ❌ no          |
| map            | ❌ no          |
| binary         | ❌ no          |
| if             | ✅ yes         |
| case           | ✅ yes         |
| receive        | ✅ yes         |
| send           | ✅ yes         |
| spawn          | ✅ yes         |
| spawn_monitor  | ❌ no          |
| match          | 🟡 partial     |
| function       | ✅ yes         |
| guards         | ❌ no          |
| register       | 🟡 static eval |
| unregister     | ❌ no          |
| whereis        | ❌ no          |
| rand:uniform   | ✅ yes         |
| self           | ✅ yes         |
| anon functions | ✅ yes         |
| try catch      | ❌ no          |
| after          | ❌ no          |
| math operation | ❌ no          |

## Credits

This project was made for the Bachelor's degree [Thesis](https://gabrielegenovese.github.io/chorer/assets/thesis.pdf) of the Computer Science course at Alma Mater Studiorum - University of Bologna. I am grateful to the professor [Ivan Lanese](http://www.cs.unibo.it/~lanese/), who supervised the development of this project.
