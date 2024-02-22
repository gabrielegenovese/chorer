# For loop receive example

## Use

```bash
./_build/default/bin/chorer ./examples/customer/customer.erl main0 ./examples/customer
```

## Description

Features:

- receive in una funzione ricorsiva
- avviene un deadlock perché il main rimane in attesa di un messaggio ma nel sistema ci sono solo due processi

## Results

LV corretta. In GV manca da implementare il sistema per capire se è in uno stato finale. Verrebbe poi notificato il deadlock!