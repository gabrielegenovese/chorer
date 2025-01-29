% #!/usr/bin/env escript

%%%------------------------------------------------------------------------------
%%% @doc
%%% The main module of the program.
%%% It initialize the ets tables and generetes the localviews and the globalview.
%%% @end
%%%------------------------------------------------------------------------------
-module(chorer).
-include("share/common_data.hrl").

%%% API
-export([main/1, generate/5]).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Define the cli arguments and run the program (using argparse).
main(Args) ->
    argparse:run(Args, cli(), #{progname => ?MODULE}).

cli() ->
    #{
        arguments => [
            #{name => input, type => string, help => "Erlang soure file"},
            #{name => entrypoint, type => {atom, unsafe}, help => "Entrypoint of the program"},
            #{name => output, type => string, default => ".", help => "Output directory for the generated dot files"},
            #{name => minl, type => boolean, default => true, help => "Minimize the localviews"},
            #{name => ming, type => boolean, default => false, help => "Minimize the globalviews"}
        ],
        help=> """
        Extract a choreography automata of an Erlang program.
        """,
        handler =>
            fun(
                #{
                    input := InputFile,
                    entrypoint := EntryPoint,
                    output := OutputDir,
                    minl := MinL,
                    ming := MinG
                }
            ) ->
                generate(InputFile, EntryPoint, OutputDir, MinL, MinG)
            end
    }.

%%% @doc
%%% Generate the localviews and the globalview specifing the output directory.
%%% It initialize the ets tables and generates the localviews and globalview.
-spec generate(InputFile, EntryPoint, OutDir, MinL, MinG) -> atom() when
    InputFile :: string(),
    EntryPoint :: atom(),
    OutDir :: string(),
    MinL :: boolean(),
    MinG :: boolean().
generate(InputFile, EntryPoint, OutDir, MinL, MinG) ->
    io:fwrite("Analysing ~p, entrypoint: ~p, output: ~p~n", [InputFile, EntryPoint, OutDir]),
    Settings = settings:new_settings(InputFile, EntryPoint, OutDir, MinL, MinG),
    db:init(Settings),
    md:extract(),
    NoError = lv:generate(),
    case NoError of
        true -> gv:generate();
        false -> done
    end,
    db:close().
