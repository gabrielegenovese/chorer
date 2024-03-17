# Sever Client example

## Use

```bash
./_build/default/bin/chorer ./examples/serverclient/serverclient.erl main0 examples/serverclient
```

## Description

Features:

- spawn in un ciclo
- passaggio di pid e messaggi

## Results

LV e GV corrette con un solo client.

## Other

Se si aggiunge nel main un client, il risulato della GV è interessante ma sbagliato.

Questo è un buon esempio per capire il problema di generalizzare le spawn, come viene affrontato nel paper di riferimento. Cosa dovrei scrivere nella local view se eseguo due volte la spawn di client? nella local view del client quando faccio self() dovrei ritornare client0/N invece che di 0 così nella global view metcha esattamente quello corrente.
