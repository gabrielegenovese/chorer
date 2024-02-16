-module(chorer).
-include("share/common_data.hrl").

%%% API
-export([main/1, generate/2, generate/3]).

%%%===================================================================
%%% API
%%%===================================================================

main([InputFile, EntryPoint, OutputDir] = _Args) ->
    generate(InputFile, list_to_atom(EntryPoint), OutputDir).

-spec generate(InputFile, EntryPoint) -> atom() when
    InputFile :: string(),
    EntryPoint :: atom().
generate(InputFile, EntryPoint) -> generate(InputFile, EntryPoint, #setting{}).

-spec generate(InputFile, EntryPoint, OutDir) -> atom() when
    InputFile :: string(),
    EntryPoint :: atom(),
    OutDir :: string().
generate(InputFile, EntryPoint, OutDir) ->
    io:fwrite("Analysing ~p, entrypoint: ~p~n", [InputFile, EntryPoint]),
    init_db(),
    Settings = #setting{output_dir = OutDir},
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
