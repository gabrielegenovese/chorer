%%%-------------------------------------------------------------------
%% @doc ChorEr public API
%% @end
%%%-------------------------------------------------------------------
-module(chorer_app).
-export([gen_chor/3, get_ast/1]).

gen_chor(InputFile, OutputDir, EntryPoint) ->
    io:format(
        "Input file: ~s~n"
        "Output directory: ~s~n",
        [InputFile, OutputDir]
    ),
    AST = get_ast(InputFile),
    _LocalViewList = gen_local_view:generate(AST, OutputDir, EntryPoint),
    % gen_global_view:generate(LocalViewList, OutputDir, EntryPoint),
    done.

%% internal functions

get_ast(File) ->
    {ok, AST} = epp_dodger:quick_parse_file(File),
    AST.
