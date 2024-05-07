-module(customer).
-export([main/0, customer/0, store/0]).

customer() ->
    store ! item,
    %%% Simulation of a user decition
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
            io:fwrite("Request received~n"),
            receive
                buy -> payment();
                more -> store()
            end
    end.

payment() ->
    receive
        payment ->
            %%% Simulation of a payment
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
    Str = spawn(?MODULE, store, []),
    register(store, Str),
    Cstm = spawn(?MODULE, customer, []),
    register(customer, Cstm),
    done.
