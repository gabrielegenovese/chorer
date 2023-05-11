-module(metadata).
-include("../common/common_data.hrl").
%%% Api
-export([extract/2]).

%%%===================================================================
%%% API
%%%===================================================================

extract(EntryPoint, InputFile) ->
    gen_ast(InputFile),
    gen_exported_fun_list(),
    gen_actor_list(EntryPoint).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

gen_ast(InputFile) ->
    {ok, AST} = epp_dodger:quick_parse_file(InputFile),
    ?DBMANAGER ! {set_ast, AST}.

gen_exported_fun_list() ->
    CodeAst = common_fun:get_ast_from_db(),
    ExpList = lists:foldl(
        fun(Line, AccList) ->
            case Line of
                {attribute, _, export, AtrList} ->
                    AccList ++ [Name || {Name, _} <- AtrList];
                _ ->
                    AccList
            end
        end,
        [],
        CodeAst
    ),
    ?DBMANAGER ! {set_exported_fun, ExpList}.

gen_actor_list(EntryPoint) ->
    CodeAst = common_fun:get_ast_from_db(),
    ActorList = gen_actor_list(EntryPoint, CodeAst),
    ?DBMANAGER ! {set_actor_list, ActorList}.

gen_actor_list(EntryPoint, CodeAst) ->
    lists:foldl(
        fun(Item, RetList) ->
            case Item of
                {function, _, Name, _, FunAst} ->
                    if
                        Name =:= EntryPoint ->
                            RetList ++ find_actor(FunAst);
                        true ->
                            RetList
                    end;
                % this is the case branch for lines like export,
                % include, compile, module, record, etc...
                _ ->
                    RetList
            end
        end,
        [EntryPoint],
        CodeAst
    ).

%%% SI ASSUME CHE LA FUNZIONE SIA NELLO STESSO FILE DI INPUT
%%% TODO: un caso realistico è che la funzione non si trovi nello stesso file ma in un altro file
%%%       quindi bisognerebbe caricare il modulo e cercare la funzione

%%% Cerca ricorsivamente tutte le spawn partendo da una funzione
find_actor(Code) when is_list(Code) ->
    lists:foldl(
        fun(Item, PrevList) ->
            RetL =
                case Item of
                    {clause, _, _, _, Content} -> find_actor(Content);
                    {match, _, _, LeftContent} -> find_actor([LeftContent]);
                    {'case', _, _, PMList} -> find_actor(PMList);
                    {'if', _, PMList} -> find_actor(PMList);
                    {'receive', _, PMList} -> find_actor(PMList);
                    {call, _, {atom, _, spawn}, [_, {atom, _, Name}, _]} -> [Name];
                    _ -> []
                end,
            PrevList ++ RetL
        end,
        [],
        Code
    ).
