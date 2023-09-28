-module(hello).
-export([main/0, customer/0, store/0]).

customer() ->
    store ! item,
    A = true,
    case A of
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
            A = true,
            case A of
                true ->
                    customer ! accepted;
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
