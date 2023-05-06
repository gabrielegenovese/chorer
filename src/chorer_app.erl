%%%-------------------------------------------------------------------
%% @doc ChorEr public API
%% @end
%%%-------------------------------------------------------------------
-module(chorer_app).
-export([gen_chor/3, get_ast/1]).

gen_chor(InputFile, OutputDir, EntryPoint) ->
    io:format(
        "Input file: ~s~n"
        "Output directory: ~s~n"
        "Entrypoint: ~p~n",
        [InputFile, OutputDir, EntryPoint]
    ),
    AST = get_ast(InputFile),
    LocalViewList = gen_local_view:generate(AST, OutputDir, EntryPoint),
    gen_global_view:generate(LocalViewList, OutputDir, EntryPoint).

%% internal functions

get_ast(File) ->
    {ok, AST} = epp_dodger:quick_parse_file(File),
    AST.
