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

% -record(local_view, {
%     name,
%     n_args,
%     ast,
%     graph,
%     current_vartex,
%     local_vars,
%     returned_var,
%     nodes,
%     transitions,
%     recv_queue
% }).

%%% Variable data structure
%%% type could be integrer, float, etc... or pid_prodId
-record(variable, {
    type = ?UNDEFINED,
    name = ?UNDEFINED,
    value = ?UNDEFINED
}).

%%% Spanwed processes data stracture
%%% name: process id
%%% called_where: in which function the spawn() is been called
%%% args_called: spawn's arguments
%%% args_local: cluase match of function
%%% local_vars: local variables
-record(spawned_proc, {
    name = ?UNDEFINED,
    called_where = ?UNDEFINED,
    args_called = ?UNDEFINED,
    args_local = ?UNDEFINED
}).

%%%
-record(branch, {
    graph = digraph:new(),
    last_vertex = 1,
    proc_pid_m = #{},
    states_m = #{}
}).

-record(proc_info, {
    proc_id,
    current_vertex = 1,
    first_marked_edges = [],
    second_marked_edges = [],
    local_vars = sets:new(),
    message_queue = []
}).

-record(message, {from, data, edge}).

%%%----END FILE----
