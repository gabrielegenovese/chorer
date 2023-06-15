-module(metadata).
-include("../common/common_data.hrl").

%%% API
-export([extract/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%% Extract the metada, that is all the Abstract Syntax Tree, the Actor
%%% List and the AST of every functions. Send them to the DBMANAGER.
extract(InputFile) ->
    gen_ast(InputFile),
    ActorList = gen_fun_ast_and_exported(),
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
    ?DBMANAGER ! {set_ast, AST},
    done.

%%% Generate the list of exported function and send it to the dbmanager.
%%% It also send the ast of every function.
-spec gen_fun_ast_and_exported() -> [string()].
gen_fun_ast_and_exported() ->
    Ast = common_fun:get_ast_from_db(),
    lists:foldl(
        fun(CodeLine, AccList) ->
            case CodeLine of
                {attribute, _, export, AtrList} ->
                    AccList ++ [Name || {Name, _NArgs} <- AtrList];
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
