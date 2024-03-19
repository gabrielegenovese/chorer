%%%------------------------------------------------------------------------------
%%% @doc
%%% The main module of the program.
%%% It initialize the ets tables and generetes the localviews and the globalview.
%%% @end
%%%------------------------------------------------------------------------------
-module(chorer).
-include("share/common_data.hrl").

%%% API
-export([main/1, generate/2, generate/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Function called when the tool is used from the CLI (Command Line Interface).
main([InputFile, EntryPoint, OutputDir] = _Args) ->
    generate(InputFile, share:ltoa(EntryPoint), OutputDir).

%%% @doc
%%% Generate the localviews and the globalview with base settings.
-spec generate(InputFile, EntryPoint) -> atom() when
    InputFile :: string(),
    EntryPoint :: atom().
generate(InputFile, EntryPoint) ->
    generate(InputFile, EntryPoint, "./").

%%% @doc
%%% Generate the localviews and the globalview specifing the output directory.
%%% It initialize the ets tables and generates the localviews and globalview.
-spec generate(InputFile, EntryPoint, OutDir) -> atom() when
    InputFile :: string(),
    EntryPoint :: atom(),
    OutDir :: string().
generate(InputFile, EntryPoint, OutDir) ->
    io:fwrite("Analysing ~p, entrypoint: ~p~n", [InputFile, EntryPoint]),
    Settings = #setting{output_dir = OutDir},
    init_db(),
    md:extract(InputFile),
    lv:generate(Settings),
    gv:generate(Settings, EntryPoint).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

init_db() ->
    ets:new(?CLINE, [set, named_table]),
    ets:new(?DBMANAGER, [set, named_table]),
    ets:new(?FUNAST, [set, named_table]),
    ets:new(?LOCALVIEW, [set, named_table]),
    ets:new(?REGISTERDB, [set, named_table]),
    ets:new(?ARGUMENTS, [set, named_table]),
    ets:new(?SPAWNC, [set, named_table]).
