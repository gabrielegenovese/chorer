%%%-------------------------------------------------------------------
%%% @doc
%%% The metadata extractor module.
%%% This module extract all the essential metadata prior to the localviews
%%% and globalview generation.
%%% @end
%%%-------------------------------------------------------------------
-module(md).
-include("../share/common_data.hrl").

%%% API
-export([extract/0, parse_file/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Extract the metadata: the AST (abstract syntax tree) for each function
%%% and all the Actor List.
extract() ->
    InputFile = settings:get(inputfile),
    gen_fun_ast_and_exported(parse_file(InputFile)).

%%% @doc
%%% Return the AST of the file localted in Path.
parse_file(Path) ->
    element(2, epp_dodger:quick_parse_file(Path)).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%% Parse all the file and save the Ast of each function and also all the possible actors.
%%% All the exported functions are cosidereted possible actors.
gen_fun_ast_and_exported(Ast) ->
    ActorList =
        lists:foldl(
            fun(CodeLine, AccActorList) ->
                case CodeLine of
                    {attribute, _, export, AtrList} ->
                        AccActorList ++ [share:merge_fun_ar(N, A) || {N, A} <- AtrList];
                    {function, Line, Name, Arity, FunAst} ->
                        % io:fwrite("[MD] Found ~p~n", [share:merge_fun_ar(Name, Arity)]),
                        ets:insert(?FUNAST, {
                            share:merge_fun_ar(Name, Arity), {function, Line, FunAst}
                        }),
                        AccActorList;
                    _ ->
                        AccActorList
                end
            end,
            [],
            Ast
        ),
    ets:insert(?DBMANAGER, {?ACTORLIST, ActorList}).
