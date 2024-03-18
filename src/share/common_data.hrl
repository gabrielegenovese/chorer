%%%----FILE common_data.hrl----

%%% CONSTANTS

-define(FINALTAG, "final").
-define(DBMANAGER, dbmanager).
-define(UNDEFINED, none).
-define(ANYDATA, any).

-define(CLINE, curr_line).
-define(ACTORLIST, actor_list).
-define(FUNAST, fun_ast).
-define(ARGUMENTS, args).
-define(LOCALVIEW, lv).
-define(REGISTERDB, reg).
-define(SPAWNC, spc).
-define(ARITYSEP, "/").
-define(SEPARATOR, ".").

%%% RECORDS

-record(setting, {
    more_info_lv = false,
    debug = false,
    output_dir = "./",
    save_all = false
}).
-type setting() :: #setting{
    more_info_lv :: boolean(),
    debug :: boolean(),
    output_dir :: string()
}.

-record(variable, {
    type = ?ANYDATA,
    name = ?UNDEFINED,
    value = ?ANYDATA
}).

-record(wip_lv, {
    fun_name = "",
    fun_ast = {},
    graph = digraph:new(),
    min_graph = digraph:new(),
    last_vertex = 1,
    local_vars = [],
    ret_var = #variable{},
    % node_map = #{},
    % input_vars = [],
    edge_map = #{},
    settings = #setting{}
}).

-record(node, {
    id,
    label,
    op,
    out_trans
}).

-record(actor, {name, arity}).

-record(branch, {
    graph = digraph:new(),
    last_vertex = 1,
    proc_pid_m = #{},
    states_m = #{}
}).

-record(actor_info, {
    fun_name = "",
    id = ?UNDEFINED,
    current_state = 1,
    first_marked_edges = [],
    second_marked_edges = [],
    spawn_vars = sets:new(),
    local_vars = sets:new(),
    message_queue = []
}).

-record(message, {from, data, edge}).

%%%----END FILE----
