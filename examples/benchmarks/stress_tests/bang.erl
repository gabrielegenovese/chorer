%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%

%%%-------------------------------------------------------------------
%%% File    : bang.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : 
%%%
%%% Created :  3 Dec 2008 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------

-module(bang).

-export([bench_args/2, run/3]).

bench_args(Version, Conf) ->
	{_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	F = case Version of
		short -> 16;
		intermediate -> 55;
		long -> 79
	end,
	[[S,M] || S <- [F * Cores], M <- [F * Cores]].

run([S,M|_], _, _) ->
	Parent = self(),
	Done   = make_ref(),
	Bang   = {make_ref(),make_ref(),make_ref(),make_ref(),make_ref()},
	Rec    = spawn_link(fun () -> rec(Bang, S*M), Parent ! Done end),
	lists:foreach(fun(_) ->
		spawn_link(fun () -> send(Rec, Bang, M) end)
	end, lists:seq(1, S)),
	receive Done -> ok end,
	ok.

send(_T, _M, 0) -> ok;
send(T, M, N)   -> T ! M, send(T, M, N-1).

rec(_M, 0) -> ok;
rec(M, N)  -> receive M -> rec(M, N-1) end.

