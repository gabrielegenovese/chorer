%%%-------------------------------------------------------------------
%%% @doc
%%% Module with function to manage the settings easily.
%%% @end
%%%-------------------------------------------------------------------
-module(settings).
-include("common_data.hrl").

%%% API
-export([
    new_settings/2,
    new_settings/4,
    get/1
]).

%%% Data structure
-record(setting, {
    %%% taken in input
    % mandatory
    inputfile,
    entrypoint,
    % optional
    minimize = false,
    output_dir = "./",
    %%% not in input
    more_info_lv = false,
    save_all = false
}).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Create a new seggings record with only the required data.
new_settings(InputFile, EntryPoint) ->
    #setting{inputfile = InputFile, entrypoint = EntryPoint}.

%%% @doc
%%% Create a new seggings record with some optional data.
new_settings(InputFile, EntryPoint, OutDir, Minimize) ->
    #setting{
        inputfile = InputFile,
        entrypoint = EntryPoint,
        output_dir = OutDir,
        minimize = Minimize
    }.

%%% @doc
%%% Get a specific data from the setting structure. Possible input:
%%% `inputfile', `entrypoint', `minimize', `output_dir', `more_info_lv', `save_all'.
get(What) ->
    Settings = get_settings(),
    case What of
        inputfile -> Settings#setting.inputfile;
        entrypoint -> Settings#setting.entrypoint;
        minimize -> Settings#setting.minimize;
        output_dir -> Settings#setting.output_dir;
        more_info_lv -> Settings#setting.more_info_lv;
        save_all -> Settings#setting.save_all;
        _ -> not_valid
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

get_settings() ->
    [{_, Settings}] = ets:lookup(?DBMANAGER, settings),
    Settings.
