-module(hello).
-export([main/0, customer/0, store/0]).

customer() ->
    store ! item,
    %%% Simulazione della decisione di un utente
    Done = true,
    case Done of
        true ->
            store ! buy,
            purchase();
        false ->
            store ! more,
            customer()
    end.

purchase() ->
    store ! payment,
    receive
        accepted -> done;
        reject -> purchase()
    end.

store() ->
    receive
        item ->
            receive
                buy -> payment();
                more -> store()
            end
    end.

payment() ->
    receive
        payment ->
            %%% Simulazione del esito del pagamento
            PaymentDone = true,
            case PaymentDone of
                true ->
                    customer ! accepted,
                    done;
                false ->
                    customer ! reject,
                    payment()
            end
    end.

main() ->
    Cstm = spawn(?MODULE, customer, []),
    Str = spawn(?MODULE, store, []),
    register(customer, Cstm),
    register(store, Str).
