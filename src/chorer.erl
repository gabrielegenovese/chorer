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
    %% default: output files in the same directory of the input file
    generate(InputFile, EntryPoint, filename:dirname(InputFile)).
    %generate(InputFile, EntryPoint, "./").

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
    init_db(Settings),
    md:extract(InputFile),
    lv:generate(),
    gv:generate(EntryPoint),
    del_db().

%%%===================================================================
%%% Internal Functions
%%%===================================================================

init_db(Settings) ->
    ets:new(?DBMANAGER, [set, named_table]),
    ets:insert(?DBMANAGER, {settings, Settings}),
    ets:new(?CLINE, [set, named_table]),
    ets:new(?FUNAST, [set, named_table]),
    ets:new(?LOCALVIEW, [set, named_table]),
    ets:new(?REGISTERDB, [set, named_table]),
    ets:new(?ARGUMENTS, [set, named_table]),
    ets:new(?SPAWNC, [set, named_table]).

del_db() ->
    ets:delete(?DBMANAGER),
    ets:delete(?CLINE),
    ets:delete(?FUNAST),
    ets:delete(?LOCALVIEW),
    ets:delete(?REGISTERDB),
    ets:delete(?ARGUMENTS),
    ets:delete(?SPAWNC).
