% #!/usr/bin/env escript

%%%------------------------------------------------------------------------------
%%% @doc
%%% The main module of the program.
%%% It initialize the ets tables and generetes the localviews and the globalview.
%%% @end
%%%------------------------------------------------------------------------------
-module(chorer).
-include("share/common_data.hrl").

-define(DEFOUTPUT, "./").
-define(DEFMINL, true).
-define(DEFMING, false).
-define(DEFGSTATE, true).

%%% API
-export([main/1, generate/2, generate/6, analyse/5]).

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
            #{
                name => output,
                type => string,
                default => ?DEFOUTPUT,
                help => "Output directory for the generated dot files"
            },
            #{
                name => ming,
                type => boolean,
                default => ?DEFMING,
                help => "Minimize the globalviews"
            },
            #{
                name => gstate,
                type => boolean,
                default => ?DEFGSTATE,
                help => "Global state are formed with previous messages"
            },
            #{
                name => minl,
                type => boolean,
                default => ?DEFMINL,
                help => "Minimize the localviews"
            }
        ],
        help => "Extract a choreography automata of an Erlang program.",
        handler =>
            fun(
                #{
                    input := InputFile,
                    entrypoint := EntryPoint,
                    output := OutputDir,
                    ming := MinG,
                    gstate := GState,
                    minl := MinL
                }
            ) ->
                generate(InputFile, EntryPoint, OutputDir, MinG, GState, MinL)
            end
    }.

%%% @doc
%%% Used within the Erlang shell (call generate/5).
-spec generate(InputFile, EntryPoint) -> atom() when
    InputFile :: string(),
    EntryPoint :: atom().
generate(InputFile, EntryPoint) ->
    generate(InputFile, EntryPoint, ?DEFOUTPUT, ?DEFMINL, ?DEFMINL, ?DEFGSTATE).

%%% @doc
%%% Generate the localviews and the globalview specifing the output directory.
%%% It initialize the ets tables and generates the localviews and globalview.
-spec generate(InputFile, EntryPoint, OutDir, MinG, GState, MinL) -> atom() when
    InputFile :: string(),
    EntryPoint :: atom(),
    OutDir :: string(),
    MinG :: boolean(),
    GState :: boolean(),
    MinL :: boolean().
generate(InputFile, EntryPoint, OutDir, MinG, GState, MinL) ->
    io:fwrite("Analysing ~p, entrypoint: ~p, output: ~p~n", [InputFile, EntryPoint, OutDir]),
    Settings = settings:new_settings(InputFile, EntryPoint, OutDir, MinL, MinG, GState),
    db:init(Settings),
    md:extract(),
    ErrorIsPresent = lv:generate(true),
    case ErrorIsPresent of
        false -> gv:generate(true);
        true -> done
    end,
    md:show_data(InputFile),
    db:close().

analyse(Code, EntryPoint, MinLV, MinGV, GStates) ->
    Settings = settings:new_settings("", EntryPoint, "", MinLV, MinGV, GStates),
    db:init(Settings),
    md:extract(Code),
    ErrorIsPresent = lv:generate(false),
    case ErrorIsPresent of
        false ->
            gv:generate(false),
            ActorLVsMap =  lv:get_all_lvs_string(),
            GVStr = gv:get_gv_str(),
            db:close(),
            maps:put("gv", GVStr, ActorLVsMap);
        true -> 
            error
    end.