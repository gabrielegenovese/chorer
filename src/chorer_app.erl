-module(chorer_app).
-include("common/common_data.hrl").

%%% API
-export([generate/2, generate/3, generate/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%% Generate the local and global view of an Erlang File source.
generate(InputFile, EntryPoint) ->
    OutputDir = "./",
    generate(InputFile, EntryPoint, OutputDir).

generate(InputFile, EntryPoint, OutputDir) ->
    generate(InputFile, EntryPoint, OutputDir, {true, false}).

-spec generate(InputFile, EntryPoint, OutputDir, Options) -> atom() when
    InputFile :: string(),
    EntryPoint :: atom(),
    OutputDir :: string(),
    Options :: [boolean()].
generate(InputFile, EntryPoint, OutputDir, Options) ->
    init_db(),
    %%% Get all the metadata info such as exported functions, spawn done and actors
    metadata:extract(InputFile, EntryPoint),
    %%% Generate local and global view and save them int the output directory
    local_view:generate(OutputDir, Options),
    global_view:generate(OutputDir, EntryPoint),
    finished.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%% Initialize code manager as a key based database
init_db() ->
    Ret = whereis(?DBMANAGER),
    case Ret of
        % if the pid of the dbmenager is not defined, initialize it
        'undefined' ->
            DBManagerPid = spawn(db_manager, loop, []),
            register(?DBMANAGER, DBManagerPid);
        % otherwise do nothing, because is already defined
        _Pid ->
            already_exist
    end.
