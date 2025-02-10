%%%-------------------------------------------------------------------
%%% @doc
%%% The metadata extractor module.
%%% This module extract all the essential metadata prior to the localviews
%%% and globalview generation.
%%% @end
%%%-------------------------------------------------------------------
-module(md).
-include("../share/common_data.hrl").

%%% API
-export([extract/0, parse_file/1, show_data/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Extract the metadata: the AST (abstract syntax tree) for each function
%%% and all the Actor List.
extract() ->
    InputFile = settings:get(inputfile),
    gen_fun_ast_and_exported(parse_file(InputFile)).

%%% @doc
%%% Return the AST of the file localted in Path.
parse_file(Path) ->
    element(2, epp_dodger:quick_parse_file(Path)).

%%% @doc
%%% Print some information about the local and global views.
show_data(InputFile) ->
    TotLine = get_tot_line(InputFile),
    io:fwrite("~nTotal numeber of lines: ~p~n", [TotLine]),
    {LocalViewData, GlobalViewMap} = get_graph_data(),
    lists:foreach(
        fun({FunName, LvMap}) ->
            io:fwrite("Data of ~p localview:~n", [FunName]),
            print_map(LvMap)
        end,
        LocalViewData
    ),
    io:fwrite("Data of global view: ~n"),
    print_map(GlobalViewMap),
    print_to_csv(TotLine, LocalViewData, GlobalViewMap).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%% Parse all the file and save the Ast of each function and also all the possible actors.
%%% All the exported functions are cosidereted possible actors.
gen_fun_ast_and_exported(Ast) ->
    ActorList =
        lists:foldl(
            fun(CodeLine, AccActorList) ->
                case CodeLine of
                    {attribute, _, export, AtrList} ->
                        AccActorList ++ [share:merge_fun_ar(N, A) || {N, A} <- AtrList];
                    {function, Line, Name, Arity, FunAst} ->
                        % io:fwrite("[MD] Found ~p~n", [share:merge_fun_ar(Name, Arity)]),
                        ets:insert(?FUNAST, {
                            share:merge_fun_ar(Name, Arity), {function, Line, FunAst}
                        }),
                        AccActorList;
                    _ ->
                        AccActorList
                end
            end,
            [],
            Ast
        ),
    ets:insert(?DBMANAGER, {?ACTORLIST, ActorList}).

get_tot_line(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, 0).

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof ->
            file:close(Device),
            Accum;
        _Line ->
            get_all_lines(Device, Accum + 1)
    end.

get_graph_data() ->
    LocalViewData = get_lv_data(),
    GlobalViewData = get_gv_data(),
    {LocalViewData, GlobalViewData}.

get_lv_data() ->
    AllLovalViewList = ets:tab2list(?LOCALVIEW),
    lists:map(
        fun({FunName, FunData}) ->
            RetMap = extract_info_from_data(FunData),
            {FunName, RetMap}
        end,
        AllLovalViewList
    ).

get_gv_data() ->
    Data = db:get(?GLOBALVIEW),
    extract_info_from_data(Data).

extract_info_from_data(Data) ->
    % G = Data#localview.min_graph,
    G = Data#localview.graph,
    GvNodes = length(digraph:vertices(G)),
    GvEdges = length(digraph:edges(G)),
    Map = maps:put(num_nodes, GvNodes, #{}),
    maps:put(num_edges, GvEdges, Map).

print_map(Map) ->
    maps:foreach(
        fun(Key, Value) ->
            io:fwrite("~p,~p~n", [Key, Value])
        end,
        Map
    ).

print_to_csv(TotLine, LocalViewData, GvMap) ->
    CsvData = ["line", integer_to_list(TotLine)],
    TempCsvData = lists:foldl(
        fun({FunName, LvMap}, Acc) ->
            BaseS = "lv_" ++ FunName,
            Acc ++ [BaseS ++ "_nodes", maps:get(num_nodes, LvMap)] ++
                [BaseS ++ "_edges", maps:get(num_edges, LvMap)]
        end,
        CsvData,
        LocalViewData
    ),
    RetCsvData =
        TempCsvData ++ ["gv_nodes", maps:get(num_nodes, GvMap)] ++
            ["gv_edges", maps:get(num_edges, GvMap)],
    D = format_to_string(RetCsvData, ""),
    Out = settings:get(output_dir),
    ok = file:write_file(Out ++ "/output.csv", unicode:characters_to_binary(D)).

format_to_string([], S) ->
    S;
format_to_string([K | T], S) ->
    [V | Rest] = T,
    format_to_string(Rest, S ++ K ++ "," ++ itol(V) ++ "\n").

itol(I) when is_list(I) ->
    I;
itol(I) when is_integer(I) ->
    integer_to_list(I).
