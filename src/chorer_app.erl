%%%-------------------------------------------------------------------
%% @doc ChorEr public API
%% @end
%%%-------------------------------------------------------------------

-module(chorer_app).

-behaviour(application).

-export([start/2, stop/1, gen_chor/2]).

start(_StartType, _StartArgs) ->
    chorer_sup:start_link().

stop(_State) ->
    ok.

gen_chor(InputFile, OutputDir) ->
    io:format("Input file: ~s~nOutput directory ~s~n", [InputFile, OutputDir]),
    AST = get_ast(InputFile),
    generate_local_views(AST),
    generate_global_view(AST),
    AST.

%% internal functions

get_ast(File) ->
    {ok, AST} = epp_dodger:quick_parse_file(File),
    AST.

generate_local_views(_AST) ->
    done.

generate_global_view(_AST) ->
    done.
