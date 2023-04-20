%%%----FILE common_data.hrl----

-define(FINALSTATE, "final").

%% fsa_states, a structure to keep track of all the state of a Finite State Automata
%% start_state: a singlular starting poin, final_states: list of states
-record(fsa_states, {all_states, start_state, final_states}).

%% graph node data, a structure to help the creation of the fsa
-record(node_data, {is_start, is_final, data, current_operation, label}).

%%%----END FILE----
