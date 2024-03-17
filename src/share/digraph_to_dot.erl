%%% @doc
%%% Module based from
%%% <a href="https://github.com/jkrukoff/digraph_export/blob/master/src/digraph_export_dot.erl">jkrukoff</a>.
%%%
%%% Features added:
%%% <ul>
%%%     <li>distinctions between initial and final states</li>
%%%     <li>changed orientation of graphs from left to right</li>
%%%     <li>enanched format</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(digraph_to_dot).
-include("common_data.hrl").

%%% API
-export([convert/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Convert a graph to a dot graph.
-spec convert(Graph, Name) -> Serialized when
    Graph :: digraph:graph(),
    Name :: unicode:charlist(),
    Serialized :: unicode:charlist().
convert(Graph, Name) ->
    Ids = ids(Graph),
    Vertices = [format_vertex(V, Ids) || V <- vertices(Graph)],
    Edges = [format_edge(E, Ids) || E <- edges(Graph)],
    format_string(
        "digraph ~ts {~n"
        % graph left to right
        "\trankdir=\"LR\";~n"
        "\tn_0 [label=\"~s\", shape=\"plaintext\"];~n"
        "~ts~n~ts}~n",
        [Name, Name, Vertices, Edges]
    ).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

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

ids(Graph) ->
    Vertices = digraph:vertices(Graph),
    maps:from_list([{V, ["n_", integer_to_list(I)]} || {I, V} <- lists:enumerate(Vertices)]).

vertices(Graph) ->
    [digraph:vertex(Graph, V) || V <- digraph:vertices(Graph)].

edges(Graph) ->
    [digraph:edge(Graph, E) || E <- digraph:edges(Graph)].

format_vertex({V, Label}, Ids) ->
    #{V := Id} = Ids,
    {NewLabel, FinalState} = is_final_state(Label),
    Str =
        case NewLabel =:= 1 of
            true -> "\n\tn_0 -> " ++ Id ++ " [arrowhead=none];";
            false -> ""
        end,
    format_string(
        "\t~ts [id=~ts, shape=~tscircle, label=\"~tp\"];~s~n",
        [Id, quoted(V), FinalState, NewLabel, Str]
    ).

format_edge({Edge, V1, V2, Label}, Ids) ->
    #{V1 := Id1} = Ids,
    #{V2 := Id2} = Ids,
    case Label =:= [] of
        true ->
            format_string(
                "\t~ts -> ~ts [id=~s];~n",
                [Id1, Id2, quoted(Edge)]
            );
        false ->
            format_string(
                "\t~ts -> ~ts [id=~ts, label=\"~ts\"];~n",
                [Id1, Id2, quoted(Edge), Label]
            )
    end.

format_string(Format, ArgList) ->
    lists:flatten(io_lib:format(Format, ArgList)).
