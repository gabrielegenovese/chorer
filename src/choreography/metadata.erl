-module(metadata).
-include("../share/common_data.hrl").

%%% API
-export([extract/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%% Extract the metada, that is the Abstract Syntax Tree's file (AST), the
%%% Actor List and the AST of every functions. Send them to the DBMANAGER.
extract(InputFile, EntryPoint) ->
    gen_ast(InputFile),
    ActorList = gen_fun_ast_and_exported(),
    %%% Send the actor list to the dbmanager
    db_manager:send_actor_list(ActorList),
    gen_spawned_names_and_args(EntryPoint),
    db_manager:reset_spawn_counter().

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%% Use of epp_dodger:quick_parse_file to generete the Abstract Syntax Tree
%%% of the InputFile and send it to the dbmanager
-spec gen_ast(InputFile) -> atom() when
    InputFile :: string().
gen_ast(InputFile) ->
    {ok, AST} = epp_dodger:quick_parse_file(InputFile),
    db_manager:send_ast(AST),
    done.

%%% Generate the list of exported function and send it to the dbmanager.
%%% It also send the ast of every function.
-spec gen_fun_ast_and_exported() -> [string()].
gen_fun_ast_and_exported() ->
    Ast = db_manager:get_ast(),
    lists:foldl(
        fun(CodeLine, AccList) ->
            case CodeLine of
                {attribute, _, export, AtrList} ->
                    AccList ++ [Name || {Name, _NArgs} <- AtrList];
                {function, _, Name, _NArgs, FunAst} ->
                    db_manager:send_fun_ast(Name, FunAst),
                    AccList;
                _ ->
                    AccList
            end
        end,
        [],
        Ast
    ).

new_spawned_proc(Name, FunName, ArgC, ArgL) ->
    #spawned_proc{
        name = Name,
        called_where = FunName,
        args_called = ArgC,
        args_local = ArgL
    }.

gen_spawned_names_and_args(EntryPoint) when is_atom(EntryPoint) ->
    L = gen_spawned_names_and_args(EntryPoint, ?UNDEFINED, true),
    db_manager:send_spawn_info(L).

%%% Get all the spawned function and the arguments passed to it.
gen_spawned_names_and_args(EntryPoint, CalledArgs, CreateEntry) when is_atom(EntryPoint) ->
    Ast = db_manager:get_fun_ast(EntryPoint),
    case Ast of
        no_func_found ->
            [];
        _ ->
            ListOfSpawn = get_spawned_loop(Ast, EntryPoint),
            Vars = get_clause_vars(EntryPoint),
            NewVal = #spawned_proc{name = EntryPoint, args_called = CalledArgs, args_local = Vars},
            case CreateEntry of
                true -> [NewVal] ++ ListOfSpawn;
                false -> ListOfSpawn
            end
    end.

%%% Extract the variables from a clause statement (ast format)
get_clause_vars(FunName) ->
    Ast = db_manager:get_fun_ast(FunName),
    [{clause, _, Vars, _, _} | _] = Ast,
    Vars.

%%% Find every spawn
get_spawned_loop(Code, FunName) when is_list(Code) ->
    lists:foldl(
        fun(Line, A) ->
            A ++
                case Line of
                    %%% Evaluate recursively statement with possible spawn in it
                    {clause, _, _, _, Content} ->
                        get_spawned_loop(Content, FunName);
                    {match, _, _VarName, LeftContent} ->
                        case is_list(LeftContent) of
                            true -> get_spawned_loop(LeftContent, FunName);
                            false -> get_spawned_loop([LeftContent], FunName)
                        end;
                    {'case', _, _, PMList} ->
                        get_spawned_loop(PMList, FunName);
                    {'if', _, PMList} ->
                        get_spawned_loop(PMList, FunName);
                    {'receive', _, PMList} ->
                        get_spawned_loop(PMList, FunName);
                    {op, _, '!', DataSentAst} ->
                        get_spawned_loop([DataSentAst], FunName);
                    %%% When a spawn is found, send the data to the db manager
                    {call, _, {atom, _, spawn}, [_, {atom, _, Name}, ArgList]} ->
                        ArgVars = get_clause_vars(Name),
                        C = db_manager:inc_spawn_counter(Name),
                        FName = list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(C)),
                        % TODO: come evitare loop infiniti?
                        FAst = db_manager:get_fun_ast(Name),
                        L = get_spawned_loop(FAst, Name),
                        [new_spawned_proc(FName, FunName, ArgList, ArgVars)] ++ L;
                    %%% Attention: do not change position of this case branch
                    {call, _, {atom, _, FunName}, _} ->
                        [];
                    %%% Generic function call
                    {call, _, {atom, _, Name}, ArgL} ->
                        gen_spawned_names_and_args(Name, ArgL, false);
                    _ ->
                        []
                end
        end,
        [],
        Code
    ).
