%%%----FILE common_data.hrl----

%%% CONSTANTS

-define(FINALTAG, "final").
-define(DBMANAGER, dbmanager).
-define(UNDEFINED, none).
-define(ANYDATA, any).

%%% DB names
-define(CLINE, curr_line).
-define(ACTORLIST, actor_list).
-define(FUNAST, fun_ast).
-define(ARGUMENTS, args).
-define(LOCALVIEW, lv).
-define(REGISTERDB, reg).
-define(SPAWNCOUNT, spc).
-define(SETTINGS, settings).

%%% Separators
-define(ARITYSEP, "/").
-define(NSEQSEP, ".").
-define(PMSEQSEP, "#").
-define(SENDSEP, "!").

%%% RECORDS

-record(variable, {
    type = ?ANYDATA,
    name = unknown,
    value = ?ANYDATA
}).

-record(localview, {
    fun_name = "",
    fun_ast = {},
    graph = digraph:new(),
    min_graph = digraph:new(),
    last_vertex = 1,
    local_vars = [],
    ret_var = #variable{},
    % additional info about the edges
    % key: label, value: depends on the edge
    % if key is a spawn label, value is spawn arguments
    % if key is a send label, value is the data sent
    % if key is a receive label, value is nothing
    edge_additional_info = #{},
    states_additional_info = #{}
}).

-record(branch, {
    graph = digraph:new(),
    last_vertex = 1,
    proc_pid_m = #{}
}).

-record(actor_info, {
    % name of the spawned function
    fun_name = "",
    % sequential number of the actor
    id = ?UNDEFINED,
    % local state of lv
    current_state = 1,
    % first filter
    first_marked_edges = [],
    % second filter
    second_marked_edges = [],
    % actor's spawing variable TODO: CHANGE
    spawn_vars = sets:new(),
    local_vars = sets:new(),
    message_queue = []
}).

%%%----END FILE----
