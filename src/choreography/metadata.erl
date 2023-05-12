-module(metadata).
-include("../common/common_data.hrl").
%%% Api
-export([extract/2]).

%%%===================================================================
%%% API
%%%===================================================================

extract(EntryPoint, InputFile) ->
    gen_ast(InputFile),
    gen_fun_ast_and_exported(),
    %%% Generate the actor list from an entry point and append also the entry point
    ActorList = [EntryPoint] ++ gen_actor_list(EntryPoint),
    %%% Send the actor list to the dbmanager
    ?DBMANAGER ! {set_actor_list, ActorList}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%% Use of epp_dodger:quick_parse_file to generete the Abstract Syntax Tree
%%% of the InputFile and send it to the dbmanager
-spec gen_ast(InputFile) -> atom() when
    InputFile :: string().
gen_ast(InputFile) ->
    {ok, AST} = epp_dodger:quick_parse_file(InputFile),
    ?DBMANAGER ! {set_ast, AST}.

%%% Generate the list of exported function and send it to the dbmanager.
%%% It also send the ast of every function.
gen_fun_ast_and_exported() ->
    Ast = common_fun:get_ast_from_db(),
    lists:foldl(
        fun(CodeLine, AccList) ->
            case CodeLine of
                {attribute, _, export, AtrList} ->
                    AccList ++ [Name || {Name, _} <- AtrList];
                {function, _, Name, _, FunAst} ->
                    ?DBMANAGER ! {set_fun_ast, Name, FunAst},
                    AccList;
                _ ->
                    AccList
            end
        end,
        [],
        Ast
    ).

%%% Generate the actor list, that is the list of function spawned from the entrypoint
-spec gen_actor_list(EntryPoint) -> [string()] when
    EntryPoint :: atom().

%%% SI ASSUME CHE LA FUNZIONE SIA NELLO STESSO FILE DI INPUT
%%% TODO: un caso realistico è che alcune funzioni non si trovino nello stesso file ma
%%%       in un altro file quindi bisognerebbe caricare il modulo e cercare la funzione

gen_actor_list(EntryPoint) ->
    FunAst = common_fun:get_fun_ast_from_db(EntryPoint),
    case FunAst of
        no_ast_found ->
            [];
        _ ->
            find_actors(EntryPoint, FunAst)
    end.

%%% Find recursively all the spawn called, starting from a function and its code
find_actors(FunName, FunCode) when is_list(FunCode) ->
    lists:foldl(
        fun(Item, PrevList) ->
            RetL =
                case Item of
                    {clause, _, _, _, Content} ->
                        find_actors(FunName, Content);
                    {match, _, _, LeftContent} ->
                        find_actors(FunName, [LeftContent]);
                    {'case', _, _, PMList} ->
                        find_actors(FunName, PMList);
                    {'if', _, PMList} ->
                        find_actors(FunName, PMList);
                    {'receive', _, PMList} ->
                        find_actors(FunName, PMList);
                    %%% If a spawn is called, append the function name to the actor list of the function
                    {call, _, {atom, _, spawn}, [_, {atom, _, Name}, _]} ->
                        [Name] ++ gen_actor_list(Name);
                    % stop recursive call
                    {call, _, {atom, _, FunName}, _} ->
                        [];
                    %%% Attention: don't change the position of this pattern matching
                    %%% If a function is called, get the actor list of the function
                    {call, _, {atom, _, Name}, _} ->
                        gen_actor_list(Name);
                    _ ->
                        []
                end,
            PrevList ++ RetL
        end,
        [],
        FunCode
    ).
