%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Serialization support for converting a digraph graph to the <a
%%% href="http://www.graphviz.org/doc/info/lang.html">dot</a> file
%%% format.
%%% @end
%%%-------------------------------------------------------------------
-module(digraph_to_dot).
-include("common_data.hrl").

%%% API
-export([convert/1, convert/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec convert(Graph) -> Serialized when
    Graph :: digraph:graph(),
    Serialized :: unicode:charlist().
convert(Graph) ->
    Ids = ids(Graph),
    Vertices = [format_vertex(V, Ids) || V <- vertices(Graph)],
    Edges = [format_edge(E, Ids) || E <- edges(Graph)],
    io_lib:format(
        "digraph global {~n"
        % graph direction left to right
        "\trankdir=\"LR\";~n"
        "\tn_0 [label=\"global\", shape=\"plaintext\"];~n"
        "~ts~n~ts}~n",
        [Vertices, Edges]
    ).

-spec convert(Graph, Name) -> Serialized when
    Graph :: digraph:graph(),
    Name :: unicode:charlist(),
    Serialized :: unicode:charlist().
convert(Graph, Name) ->
    Ids = ids(Graph),
    Vertices = [format_vertex(V, Ids) || V <- vertices(Graph)],
    Edges = [format_edge(E, Ids) || E <- edges(Graph)],
    io_lib:format(
        "digraph ~ts{~n"
        % graph left to right
        "\trankdir=\"LR\";~n"
        "\tn_0 [label=\"~s\", shape=\"plaintext\"];~n"
        "~ts~n~ts}~n",
        [pad(Name), Name, Vertices, Edges]
    ).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

pad("") ->
    "";
pad(String) ->
    [String, " "].

is_final_state(Label) when not is_integer(Label) ->
    NewLabel = re:replace(Label, ?FINALTAG, "", [{return, list}]),
    {list_to_integer(NewLabel), "double"};
is_final_state(L) when is_integer(L) ->
    {L, ""}.

quoted(Term) ->
    String = io_lib:format("~tw", [Term]),
    % Order matters here.
    Slashes = string:replace(String, "\\", "\\\\"),
    Quotes = string:replace(Slashes, "\"", "\\\""),
    Final = string:replace(Quotes, "\'", "", all),
    [$", Final, $"].

enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).

ids(Graph) ->
    Vertices = digraph:vertices(Graph),
    maps:from_list([{V, ["n_", integer_to_list(I)]} || {I, V} <- enumerate(Vertices)]).

vertices(Graph) ->
    [digraph:vertex(Graph, V) || V <- digraph:vertices(Graph)].

edges(Graph) ->
    [digraph:edge(Graph, E) || E <- digraph:edges(Graph)].

format_vertex({V, Label}, Ids) ->
    #{V := Id} = Ids,
    {NewLabel, FinalState} = is_final_state(Label),
    if
        NewLabel =:= 1 -> Str = "\n\tn_0 -> " ++ Id ++ " [arrowhead=none];";
        true -> Str = ""
    end,
    io_lib:format(
        "\t~ts [id=~ts, shape=~tscircle, label=~ts];~s~n",
        [Id, quoted(V), FinalState, quoted(NewLabel), Str]
    ).

format_edge({Edge, V1, V2, Label}, Ids) ->
    #{V1 := Id1} = Ids,
    #{V2 := Id2} = Ids,
    if
        Label =:= [] ->
            io_lib:format(
                "\t~ts -> ~ts [id=~ts];~n",
                [Id1, Id2, quoted(Edge)]
            );
        true ->
            io_lib:format(
                "\t~ts -> ~ts [id=~ts, label=~ts];~n",
                [Id1, Id2, quoted(Edge), quoted(Label)]
            )
    end.
