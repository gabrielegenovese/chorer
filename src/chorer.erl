%%%------------------------------------------------------------------------------
%%% @doc
%%% The main module of the program.
%%% It initialize the ets tables and generetes the localviews and the globalview.
%%% @end
%%%------------------------------------------------------------------------------
-module(chorer).
-include("share/common_data.hrl").

%%% API
-export([main/1, generate/2, generate/5]).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Function called when the tool is used from the CLI (Command Line Interface).
main([InputFile, EntryPoint, OutputDir] = _Args) ->
    generate(InputFile, share:ltoa(EntryPoint), OutputDir, true, false);
main([InputFile, EntryPoint, OutputDir, Minimize] = _Args) ->
    generate(InputFile, share:ltoa(EntryPoint), OutputDir, share:ltoa(Minimize), false);
main([InputFile, EntryPoint, OutputDir, MiniL, MinG] = _Args) ->
    generate(InputFile, share:ltoa(EntryPoint), OutputDir, share:ltoa(MiniL), share:ltoa(MinG)).

%%% @doc
%%% Generate the localviews and the globalview with base settings.
-spec generate(InputFile, EntryPoint) -> atom() when
    InputFile :: string(),
    EntryPoint :: atom().
generate(InputFile, EntryPoint) ->
    generate(InputFile, EntryPoint, filename:dirname(InputFile), true, false).

%%% @doc
%%% Generate the localviews and the globalview specifing the output directory.
%%% It initialize the ets tables and generates the localviews and globalview.
-spec generate(InputFile, EntryPoint, OutDir, MiniL, MinG) -> atom() when
    InputFile :: string(),
    EntryPoint :: atom(),
    OutDir :: string(),
    MiniL :: boolean(),
    MinG :: boolean().
generate(InputFile, EntryPoint, OutDir, MiniL, MinG) ->
    io:fwrite("Analysing ~p, entrypoint: ~p~n", [InputFile, EntryPoint]),
    Settings = settings:new_settings(InputFile, EntryPoint, OutDir, MiniL, MinG),
    db:init(Settings),
    md:extract(),
    NoError = lv:generate(),
    case NoError of
        true -> gv:generate();
        false -> done
    end,
    db:close().
