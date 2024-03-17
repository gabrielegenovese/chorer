# Airline example

## Use

```bash
./_build/default/bin/chorer ./examples/airline/airline.erl main0 ./examples/airline
```

## Description

Features:

- `seat` è una funzione con due definizioni e grazie alla funzione `eval:function_list` viene gestito correttamente questo caso ricominciando la costruzione del grafo dal primo vertice
- nel main viene chiamata la funzione `seat` che viene correttamente attaccata al main grazie alla funzione merge_graph (non viene valutato il passaggio di variabili)
- durante la costruzione della gv, vengono correttamente valutati i passaggi di variabili delle spawn e delle send/recv

## Results

La GV non sembra corretta, il problema sembra essere che torni in stati precedenti quando non dovrebbe essere così.
La LV del main è molto generica, potrebbe essere più precisa se si esegue la valuzione della variabile 2 (viene allegata una LV corretta nella cartella).
