-module(actor_emul).
-include("../share/common_data.hrl").

%%% API
-export([
    proc_loop/1,
    use_proc_transition/2,
    get_proc_edges/1,
    get_proc_out_degree/1,
    get_proc_edge_info/2,
    get_proc_localvars/1,
    add_proc_localvars/2,
    get_proc_data/1,
    set_proc_data/2,
    get_proc_mess_queue/1,
    add_proc_mess_queue/2,
    del_proc_mess_queue/2
]).

%%%===================================================================
%%% API
%%%===================================================================

use_proc_transition(P, E) -> P ! {use_transition, E}.
get_proc_edges(P) -> send_recv(P, {self(), get_edges}).
get_proc_out_degree(P) -> send_recv(P, {self(), get_out_degree}).
get_proc_edge_info(P, E) -> send_recv(P, {self(), get_edge_info, E}).
get_proc_localvars(P) -> send_recv(P, {self(), get_local_vars}).
add_proc_localvars(P, V) -> P ! {add_local_var, V}.
get_proc_data(P) -> send_recv(P, {self(), get_data}).
set_proc_data(P, Data) -> P ! {set_data, Data}.
get_proc_mess_queue(P) -> send_recv(P, {self(), get_mess_queue}).
add_proc_mess_queue(P, M) -> P ! {add_mess_queue, M}.
del_proc_mess_queue(P, M) -> P ! {del_mess_queue, M}.

send_recv(P, Data) ->
    P ! Data,
    receive
        {D} -> D
    end.

proc_loop(Data) ->
    ProcName = Data#actor_info.proc_id,
    G = common_fun:get_localview(ProcName),
    % timer:sleep(200),
    VCurr = Data#actor_info.current_state,
    FirstMarkedE = Data#actor_info.first_marked_edges,
    SecondMarkedE = Data#actor_info.second_marked_edges,
    MessageQueue = Data#actor_info.message_queue,
    LocalVars = Data#actor_info.local_vars,
    receive
        {use_transition, E} ->
            IsAlreadyMarkedOnce = lists:member(E, FirstMarkedE),
            case digraph:edge(G, E) of
                {E, VCurr, VNew, _} when IsAlreadyMarkedOnce ->
                    {_, FromLabel} = digraph:vertex(G, VCurr),
                    {_, ToLabel} = digraph:vertex(G, VNew),
                    NewL =
                        case ToLabel =< FromLabel of
                            true ->
                                % io:fwrite("[PROC LOOP] RESET LOCALV IN ~p~n", [ProcName]),
                                sets:new();
                            false ->
                                LocalVars
                        end,
                    proc_loop(Data#actor_info{
                        current_state = VNew,
                        second_marked_edges = SecondMarkedE ++ [E],
                        local_vars = NewL
                    });
                {E, VCurr, VNew, _} ->
                    {_, FromLabel} = digraph:vertex(G, VCurr),
                    {_, ToLabel} = digraph:vertex(G, VNew),
                    NewL =
                        case ToLabel =< FromLabel of
                            true ->
                                % io:fwrite("[PROC LOOP] RESET LOCALV IN ~p~n", [ProcName]),
                                sets:new();
                            false ->
                                LocalVars
                        end,
                    proc_loop(Data#actor_info{
                        current_state = VNew,
                        first_marked_edges = FirstMarkedE ++ [E],
                        local_vars = NewL
                    });
                _ ->
                    io:fwrite("[PROC LOOP] V ~p Edge ~p non trovato in ~p~n", [VCurr, E, ProcName]),
                    proc_loop(Data)
            end;
        {P, get_edges} ->
            EL = digraph:out_edges(G, VCurr),
            ERet = filter_marked_edges(EL, SecondMarkedE),
            P ! {ERet},
            proc_loop(Data);
        {P, get_out_degree} ->
            P ! {digraph:out_degree(G, VCurr)},
            proc_loop(Data);
        {P, get_edge_info, E} ->
            P ! {digraph:edge(G, E)},
            proc_loop(Data);
        {P, get_data} ->
            P ! {Data},
            proc_loop(Data);
        {set_data, NewData} ->
            proc_loop(NewData);
        {P, get_local_vars} ->
            P ! {sets:to_list(LocalVars)},
            proc_loop(Data);
        {add_local_var, V} ->
            proc_loop(Data#actor_info{local_vars = sets:add_element(V, LocalVars)});
        {P, get_mess_queue} ->
            P ! {MessageQueue},
            proc_loop(Data);
        {add_mess_queue, M} ->
            proc_loop(Data#actor_info{message_queue = MessageQueue ++ [M]});
        {del_mess_queue, M} ->
            proc_loop(Data#actor_info{message_queue = lists:delete(M, MessageQueue)});
        stop ->
            ok
    end.

filter_marked_edges(EdgeL, MarkedE) -> [E || E <- EdgeL, not lists:member(E, MarkedE)].
