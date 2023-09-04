%%%----FILE common_data.hrl----

-define(FINALTAG, "final").
-define(DBMANAGER, dbmanager).
-define(UNDEFINED, none).

%%% fsa_states, a structure to keep track of all the state of a Finite State Automata
%%% start_state: a singlular starting poin, final_states: list of states
% -record(fsa_data, {all_states, start_state, final_states, transitions, labels}).

%%% graph node data, a structure to help the creation of the fsa
% -record(node_data, {
%     is_start = false,
%     is_final = false,
%     guard = [],
%     current_operation = none,
%     label = 'ɛ'
% }).

-record(local_view, {
    name,
    n_args,
    ast,
    graph,
    current_vartex,
    local_vars,
    returned_var,
    nodes,
    transitions,
    recv_queue
}).

-record(variable, {
    type = ?UNDEFINED,
    name = ?UNDEFINED,
    value = ?UNDEFINED
}).

%%% Struttura dati che indica quali processi sono stati spawnati, in quale funzione è stata
%%% chiamata, quali argomenti aveva la spawn e a quali dovrebbero corrispondere nella funzione
-record(spawned_proc, {
    name = ?UNDEFINED,
    called_where = ?UNDEFINED,
    args_called = ?UNDEFINED,
    args_local = ?UNDEFINED,
    local_vars = []
}).

%%%----END FILE----
