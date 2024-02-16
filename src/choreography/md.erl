-module(md).
-include("../share/common_data.hrl").

%%% API
-export([extract/1, parse_file/1]).

%%%===================================================================
%%% API
%%%===================================================================

extract(InputFile) ->
    gen_ast(InputFile),
    gen_fun_ast_and_exported().

parse_file(Path) -> element(2, epp_dodger:quick_parse_file(Path)).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

gen_ast(InputFile) ->
    Ast = parse_file(InputFile),
    ets:insert(?DBMANAGER, {?INPUTAST, Ast}).

gen_fun_ast_and_exported() ->
    [{_, Ast}] = ets:lookup(?DBMANAGER, ?INPUTAST),
    List =
        lists:foldl(
            fun(CodeLine, AccList) ->
                case CodeLine of
                    {attribute, _, export, AtrList} ->
                        AccList ++ [share:merge_fun_ar(N, A) || {N, A} <- AtrList];
                    {function, Line, Name, Arity, FunAst} ->
                        % io:fwrite("[MD] Found ~p~n", [share:merge_fun_ar(Name, Arity)]),
                        ets:insert(?FUNAST, {share:merge_fun_ar(Name, Arity), {function, Line, FunAst}}),
                        AccList;
                    _ ->
                        AccList
                end
            end,
            [],
            Ast
        ),
    ets:insert(?DBMANAGER, {?ACTORLIST, List}).
