# Hello example

## Use

```bash
./_build/default/bin/chorer ./examples/hello/hello.erl greet0 examples/hello
```

## Description

Features:

- uso di self per autoinviarsi messaggi
- simulaizione dell'if nondeterministico dove in un ramo concludo e nell'altro vado in ricorsione

## Results

LV giusta, GV sbagliata perché nello stato 2 non esegue la seconda send e va direttaente alla recv.
Non è stato implementata la biforcazione quando in uno stato c'è contemporaneamente una recv o un altra transizione.