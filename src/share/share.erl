%%%-------------------------------------------------------------------
%%% @doc
%%% Module with some common functions used everywhere in the project.
%%% @end
%%%-------------------------------------------------------------------
-module(share).
-include("common_data.hrl").

%%% API
-export([
    first/1,
    save_graph_to_file/4,
    add_vertex/1,
    del_vertex/2,
    is_erlvar/1,
    is_uppercase/1,
    get_fun_ast/1,
    get_localview/1,
    get_graph/1,
    get_edgedata/1,
    warning/3,
    error/3,
    get_base_label/2,
    merge_fun_ar/2,
    should_minimize/1,
    save_graph/4,
    remove_last/1,
    find_var/2,
    if_final_get_n/1,
    inc_spawn_counter/1,
    remove_counter/1,
    atol/1,
    ltoa/1
]).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Pick first element from a list
first([]) -> [];
first([H | _]) -> H.

save_graph_to_file(Graph, Dir, FileName, Type) ->
    StringName = format_name(FileName),
    case Type of
        local ->
            GraphDotStr = digraph_to_dot:convert(Graph, StringName),
            FilePath = filename:join([Dir, format_local_name(StringName)]);
        global ->
            GraphDotStr = digraph_to_dot:convert(Graph, "global"),
            FilePath = filename:join([Dir, format_global_name(StringName)])
    end,
    ToWriteData = unicode:characters_to_binary(GraphDotStr),
    file:make_dir(Dir),
    file:write_file(FilePath, ToWriteData).

-spec add_vertex(G) -> digraph:vertex() when G :: digraph:graph().
add_vertex(G) ->
    Label = new_label(G),
    digraph:add_vertex(G, Label, Label).

del_vertex(G, V) ->
    % TODO questa funzione cambia solo le label, l'ideale sarebbe cambiare anche il contenuto del vertice ma è difficile da fare
    [digraph:add_vertex(G, Ver, Ver - 1) || Ver <- digraph:vertices(G), Ver > V],
    digraph:del_vertex(G, V).

%%% @doc
%%% Return true if first letter's atom is uppercase (it's a variable in erlang),
%%% otherwise false.
is_erlvar(Name) ->
    SName = atol(Name),
    [FirstChar | _] = SName,
    is_uppercase([FirstChar]).

%%% @doc
%%% If the input character is uppercase return true, otherwise false.
is_uppercase(Char) when (is_list(Char)) and (length(Char) =:= 1) ->
    (Char >= "A") and (Char =< "Z").

get_fun_ast(FunName) ->
    Ast = ets:lookup(?FUNAST, atol(FunName)),
    case Ast of
        [] ->
            % io:fwrite("[S] Not Found in funast ~p~n", [FunName]),
            not_found;
        [{_, A}] ->
            A
    end.

get_localview(FunName) ->
    Ast = ets:lookup(?LOCALVIEW, atol(FunName)),
    case Ast of
        [] ->
            % io:fwrite("[S] Not Found in localview ~p~n", [FunName]),
            not_found;
        [{_, A}] ->
            A
    end.

get_graph(FunName) ->
    Ast = ets:lookup(?LOCALVIEW, atol(FunName)),
    case Ast of
        [] ->
            io:fwrite("[S] Not Found in graph ~p~n", [FunName]),
            not_found;
        [{_, A}] ->
            A#localview.graph
    end.

get_edgedata(FunName) ->
    Ast = ets:lookup(?LOCALVIEW, atol(FunName)),
    case Ast of
        [] ->
            io:fwrite("[S] Not Found in edgedata ~p~n", [FunName]),
            not_found;
        [{_, A}] ->
            A#localview.edge_additional_info
    end.

warning(String, Content, RetData) ->
    [{_, Line}] = ets:lookup(?CLINE, line),
    io:fwrite("[LV] WARNING on line ~p: " ++ String ++ " ~p~n", [Line, Content]),
    RetData#localview{ret_var = #variable{}}.

error(String, Content, RetData) ->
    io:fwrite("ERROR: ~p ~p~n", [String, Content]),
    RetData.

get_base_label(SetPm, Label) ->
    case SetPm of
        true -> Label;
        false -> epsilon()
    end.

ltoa(L) when is_list(L) -> list_to_atom(L);
ltoa(L) when is_atom(L) -> L.
atol(A) when is_atom(A) -> atom_to_list(A);
atol(A) when is_list(A) -> A;
atol(A) when is_number(A) -> integer_to_list(A).

if_final_get_n(L) when not is_integer(L) ->
    NewL = re:replace(L, ?FINALTAG, "", [{return, list}]),
    list_to_integer(NewL);
if_final_get_n(L) when is_integer(L) ->
    L.

merge_fun_ar(Name, Arity) ->
    atol(Name) ++ ?ARITYSEP ++ integer_to_list(Arity).

should_minimize(S) ->
    io:fwrite("Minimize ~s view? [y/n] ", [S]),
    {ok, [In]} = io:fread("", "~a"),
    case In of
        n -> false;
        _ -> true
    end.

save_graph(Data, Settings, FunName, Mode) ->
    OutputDir = Settings#setting.output_dir,
    Minimize =
        case Mode of
            global -> false;
            _ -> true
        end,
    % Minimize = should_minimize(atol(FunName) ++ " " ++ atol(Mode)),
    ToSaveG =
        case Minimize of
            true -> Data#localview.min_graph;
            false -> Data#localview.graph
        end,
    save_graph_to_file(ToSaveG, OutputDir, FunName, Mode).

remove_counter(S) ->
    [Rest, _] = string:split(atol(S), "."),
    Rest.

%%% @doc
%%% Remove the last element from a list.
remove_last(Item) ->
    ItemList = atol(Item),
    {Rest, _} = lists:split(length(ItemList) - 1, ItemList),
    Rest.

find_var([], _) ->
    not_found;
find_var([Var | Tail], Name) ->
    Cond = Var#variable.name =:= Name,
    case Cond of
        true -> Var;
        false -> find_var(Tail, Name)
    end;
find_var(Data, Name) ->
    LL = Data#localview.local_vars,
    find_var(LL, Name).

inc_spawn_counter(Name) ->
    Ret =
        case ets:lookup(?SPAWNC, share:ltoa(Name)) of
            [] -> 0;
            [{_, N}] -> N
        end,
    ets:insert(?SPAWNC, {Name, Ret + 1}),
    Ret.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

format_name(Name) -> string:replace(atol(Name), "/", "_").

format_local_name(Name) -> format_name(Name) ++ "_local_view.dot".
format_global_name(Name) -> format_name(Name) ++ "_global_view.dot".

%%% Get a new label for a given graph
new_label(Graph) -> length(digraph:vertices(Graph)) + 1.

epsilon() -> 'ɛ'.
