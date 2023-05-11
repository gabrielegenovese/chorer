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
    ActorList = [EntryPoint] ++ gen_actor_list(EntryPoint),
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

%%% Generate the list of exported function and send it to the dbmanager
%%% It also send the ast of every function
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
gen_actor_list(EntryPoint) ->
    FunAst = common_fun:get_fun_ast_from_db(EntryPoint),
    case FunAst of
        no_ast_found ->
            [];
        _ ->
            find_actor(EntryPoint, FunAst)
    end.

%%% SI ASSUME CHE LA FUNZIONE SIA NELLO STESSO FILE DI INPUT
%%% TODO: un caso realistico è che alcune funzioni non si trovino nello stesso file ma
%%%       in un altro file quindi bisognerebbe caricare il modulo e cercare la funzione

%%% Find recursively all the spawn called in
find_actor(FunName, FunCode) when is_list(FunCode) ->
    lists:foldl(
        fun(Item, PrevList) ->
            RetL =
                case Item of
                    {clause, _, _, _, Content} -> find_actor(FunName, Content);
                    {match, _, _, LeftContent} -> find_actor(FunName, [LeftContent]);
                    {'case', _, _, PMList} -> find_actor(FunName, PMList);
                    {'if', _, PMList} -> find_actor(FunName, PMList);
                    {'receive', _, PMList} -> find_actor(FunName, PMList);
                    {call, _, {atom, _, spawn}, [_, {atom, _, Name}, _]} -> [Name];
                    % stop recursive call
                    {call, _, {atom, _, FunName}, _} -> [];
                    {call, _, {atom, _, Name}, _} -> gen_actor_list(Name);
                    _ -> []
                end,
            PrevList ++ RetL
        end,
        [],
        FunCode
    ).
