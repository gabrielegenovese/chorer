%%%-------------------------------------------------------------------
%%% @doc
%%% This module simulate an actor's localview during a globalview.
%%% @end
%%%-------------------------------------------------------------------
-module(actor_emul).
-include("../share/common_data.hrl").

%%% API
-export([
    proc_loop/1,
    use_proc_transition/2,
    get_proc_edges/1,
    get_proc_edge_info/2,
    get_proc_localvars/1,
    add_proc_spawnvars/2,
    add_proc_localvars/2,
    get_proc_data/1,
    set_proc_data/2,
    get_proc_mess_queue/1,
    add_proc_mess_queue/2,
    del_proc_mess_queue/2,
    empty_filter_proc/1
]).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Use a transition of the localview.
use_proc_transition(P, E) -> P ! {use_transition, E}.

%%% @doc
%%% Get all the localview's current edges.
get_proc_edges(P) -> send_recv(P, {self(), get_edges}).

%%% @doc
%%% Get the informations of a localview's edge.
get_proc_edge_info(P, E) -> send_recv(P, {self(), get_edge_info, E}).

%%% @doc
%%% Get the list of the local variables.
get_proc_localvars(P) -> send_recv(P, {self(), get_local_vars}).

%%% @doc
%%% Add a variable to the spawn arguments of the localview.
add_proc_spawnvars(P, V) -> P ! {add_spawn_var, V}.

%%% @doc
%%% Add a variable to the local variables of the localview.
add_proc_localvars(P, V) -> P ! {add_local_var, V}.

%%% @doc
%%% Get the all the data of the process.
get_proc_data(P) -> send_recv(P, {self(), get_data}).

%%% @doc
%%% Set the all the data of the process.
set_proc_data(P, Data) -> P ! {set_data, Data}.

%%% @doc
%%% Get the message queue of the process.
get_proc_mess_queue(P) -> send_recv(P, {self(), get_mess_queue}).

%%% @doc
%%% Add a message to the message queue of the process.
add_proc_mess_queue(P, M) -> P ! {add_mess_queue, M}.

%%% @doc
%%% Delete a message from the message queue of the process.
del_proc_mess_queue(P, M) -> P ! {del_mess_queue, M}.

%%% @doc
%%% Delete process filters
empty_filter_proc(P) -> P ! {empty_filters}.

%%% @doc
%%% Loop function to simulate a process.
proc_loop(Data) ->
    ProcName = Data#actor_info.fun_name,
    % io:fwrite("[EMUL] ID ~p~n", [ProcName]),
    LV = share:get_localview(ProcName),
    G = LV#localview.min_graph,
    % timer:sleep(200),
    VCurr = Data#actor_info.current_state,
    SecondMarkedE = Data#actor_info.second_marked_edges,
    MessageQueue = Data#actor_info.message_queue,
    SpawnVars = Data#actor_info.spawn_vars,
    LocalVars = sets:union(Data#actor_info.local_vars, SpawnVars),
    receive
        {use_transition, E} ->
            manage_use_transition(Data, G, E);
        {P, get_edges} ->
            EL = digraph:out_edges(G, VCurr),
            %% APPROX: filter out edges used two times
            case EL of
                [] ->
                    P ! {final_state, []};
                _ ->
                    ERet = filter_marked_edges(EL, SecondMarkedE),
                    P ! {filtered, ERet}
            end,
            proc_loop(Data);
        {P, get_edge_info, E} ->
            P ! digraph:edge(G, E),
            proc_loop(Data);
        {P, get_data} ->
            P ! Data,
            proc_loop(Data);
        {set_data, NewData} ->
            proc_loop(NewData);
        {P, get_local_vars} ->
            P ! sets:to_list(LocalVars),
            proc_loop(Data);
        {add_spawn_var, V} ->
            proc_loop(Data#actor_info{spawn_vars = sets:add_element(V, SpawnVars)});
        {add_local_var, V} ->
            io:fwrite("local v~p~n", [Data#actor_info.local_vars]),
            proc_loop(Data#actor_info{local_vars = sets:add_element(V, LocalVars)});
        {P, get_mess_queue} ->
            P ! MessageQueue,
            proc_loop(Data);
        {add_mess_queue, M} ->
            proc_loop(Data#actor_info{message_queue = MessageQueue ++ [M]});
        {del_mess_queue, M} ->
            proc_loop(Data#actor_info{message_queue = lists:delete(M, MessageQueue)});
        {empty_filters} ->
            proc_loop(Data#actor_info{first_marked_edges = [], second_marked_edges = []});
        stop ->
            ok
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

manage_use_transition(Data, G, E) ->
    VCurr = Data#actor_info.current_state,
    FirstMarkedE = Data#actor_info.first_marked_edges,
    SecondMarkedE = Data#actor_info.second_marked_edges,
    SpawnVars = Data#actor_info.spawn_vars,
    LocalVars = sets:union(Data#actor_info.local_vars, SpawnVars),
    IsAlreadyMarkedOnce = lists:member(E, FirstMarkedE),
    case digraph:edge(G, E) of
        {E, VCurr, VNew, _} when IsAlreadyMarkedOnce ->
            NewL = check_recursion(G, VCurr, VNew, LocalVars),
            proc_loop(Data#actor_info{
                current_state = VNew,
                second_marked_edges = SecondMarkedE ++ [E],
                local_vars = NewL
            });
        {E, VCurr, VNew, _} ->
            NewL = check_recursion(G, VCurr, VNew, LocalVars),
            proc_loop(Data#actor_info{
                current_state = VNew,
                first_marked_edges = FirstMarkedE ++ [E],
                local_vars = NewL
            });
        _ ->
            io:fwrite("[EMUL] From vetex ~p Edge ~p not found~n", [VCurr, E]),
            proc_loop(Data)
    end.

check_recursion(G, VCurr, VNew, LocalVars) ->
    {_, FromLabel} = digraph:vertex(G, VCurr),
    {_, ToLabel} = digraph:vertex(G, VNew),
    To = share:if_final_get_n(ToLabel),
    From = share:if_final_get_n(FromLabel),
    case To =< From of
        true ->
            % io:fwrite("[EMUL] RESET LOCALV IN ~p from ~p to ~p~n", [
            %     ProcName, FromLabel, ToLabel
            % ]),
            % LocalVars;
            sets:new();
        false ->
            LocalVars
    end.

filter_marked_edges(EdgeL, MarkedE) -> [E || E <- EdgeL, not lists:member(E, MarkedE)].

send_recv(P, Data) ->
    P ! Data,
    receive
        D -> D
    end.
