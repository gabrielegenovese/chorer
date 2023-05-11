%%%----FILE common_data.hrl----

-define(FINALSTATE, "final").
-define(DBMANAGER, dbmanager).

%%% fsa_states, a structure to keep track of all the state of a Finite State Automata
%%% start_state: a singlular starting poin, final_states: list of states
-record(fsa_data, {all_states, start_state, final_states, transitions, labels}).

%%% graph node data, a structure to help the creation of the fsa
-record(node_data, {
    is_start = false,
    is_final = false,
    guard = [],
    current_operation = none,
    label = 'ɛ'
}).

%%%----END FILE----
