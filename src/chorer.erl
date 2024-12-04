%%%------------------------------------------------------------------------------
%%% @doc
%%% The main module of the program.
%%% It initialize the ets tables and generetes the localviews and the globalview.
%%% @end
%%%------------------------------------------------------------------------------
-module(chorer).
-include("share/common_data.hrl").

%%% API
-export([main/1, generate/2, generate/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Function called when the tool is used from the CLI (Command Line Interface).
main([InputFile, EntryPoint, OutputDir] = _Args) ->
    generate(InputFile, share:ltoa(EntryPoint), OutputDir, true);
main([InputFile, EntryPoint, OutputDir, Minimize] = _Args) ->
    generate(InputFile, share:ltoa(EntryPoint), OutputDir, share:ltoa(Minimize)).

%%% @doc
%%% Generate the localviews and the globalview with base settings.
-spec generate(InputFile, EntryPoint) -> atom() when
    InputFile :: string(),
    EntryPoint :: atom().
generate(InputFile, EntryPoint) ->
    generate(InputFile, EntryPoint, filename:dirname(InputFile), false).

%%% @doc
%%% Generate the localviews and the globalview specifing the output directory.
%%% It initialize the ets tables and generates the localviews and globalview.
-spec generate(InputFile, EntryPoint, OutDir, Minimize) -> atom() when
    InputFile :: string(),
    EntryPoint :: atom(),
    OutDir :: string(),
    Minimize :: boolean().
generate(InputFile, EntryPoint, OutDir, Minimize) ->
    io:fwrite("Analysing ~p, entrypoint: ~p~n", [InputFile, EntryPoint]),
    Settings = settings:new_settings(InputFile, EntryPoint, OutDir, Minimize),
    db:init(Settings),
    md:extract(),
    NoError = lv:generate(),
    case NoError of
        true -> gv:generate();
        false -> done
    end,
    db:close().
