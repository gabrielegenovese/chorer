-module(chorer_app).
-include("common/common_data.hrl").

%%% API
-export([generate_chor_automata/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%% Generate the local and global view of an Erlang File source.
-spec generate_chor_automata(InputFile, OutputDir, EntryPoint) -> atom() when
    InputFile :: string(),
    OutputDir :: string(),
    EntryPoint :: atom().
generate_chor_automata(InputFile, OutputDir, EntryPoint) ->
    io:format("Entrypoint: ~p~n", [EntryPoint]),
    init_db(),
    %%% Get all the metadata info such as exported functions, spawn done and actors
    metadata:extract(InputFile),
    %%% Generate local and global view and save them int the output directory
    local_view:generate(OutputDir),
    global_view:generate(OutputDir, EntryPoint).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%% Initialize code manager as a key based database
init_db() ->
    Ret = whereis(?DBMANAGER),
    case Ret of
        % if the pid of the dbmenager is not defined, initialize it
        'undefined' ->
            DBMPid = spawn(map_manager, loop, []),
            register(?DBMANAGER, DBMPid);
        % otherwise do nothing, because is already defined
        _ ->
            ?UNDEFINED
    end.
