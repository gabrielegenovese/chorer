%%%-------------------------------------------------------------------
%% @doc ChorEr public API
%% @end
%%%-------------------------------------------------------------------
-module(chorer_app).
-export([gen_chor/2, get_ast/1]).

gen_chor(InputFile, OutputDir) ->
    io:format(
        "Input file: ~s~n"
        "Output directory: ~s~n",
        [InputFile, OutputDir]
    ),
    AST = get_ast(InputFile),
    ok = gen_local_view:generate(AST, OutputDir),
    ok = gen_global_view:generate(AST, OutputDir),
    % todo remove in the future
    AST.

%% internal functions

get_ast(File) ->
    {ok, AST} = epp_dodger:quick_parse_file(File),
    AST.
