-module(chorer_app).
-include("common/common_data.hrl").

%%% API
-export([generate_chor_automata/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec generate_chor_automata(InputFile, OutputDir, EntryPoint) -> no_entry_point | done when
    InputFile :: string(),
    OutputDir :: string(),
    EntryPoint :: atom().
generate_chor_automata(InputFile, OutputDir, EntryPoint) ->
    io:format("Entrypoint: ~p~n", [EntryPoint]),
    % initialize code manager as a key based database
    init_db(),
    % get all the metadata info such as exported functions, spawn done and actors
    metadata:extract(EntryPoint, InputFile),
    % generate local and global view and save them int the output directory
    local_view:generate(OutputDir),
    global_view:generate(OutputDir, EntryPoint).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

init_db() ->
    DBMPid = spawn(map_manager, loop, []),
    register(?DBMANAGER, DBMPid).
