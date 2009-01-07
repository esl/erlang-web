%% The contents of this file are subject to the Erlang Web Public License,
%% Version 1.0, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Web Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang-consulting.com/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Erlang Training & Consulting
%% Ltd. Portions created by Erlang Training & Consulting Ltd are Copyright 2008,
%% Erlang Training & Consulting Ltd. All Rights Reserved.

%%%-------------------------------------------------------------------
%%% File    : e_cluster.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @hidden
%%% @end
%%%-------------------------------------------------------------------

-module(e_cluster).
-export([inform_fe_servers/0, dispatcher_reload/0, invalidate/1]).

inform_fe_servers() ->
    Fun = fun(Server) ->
		  rpc:cast(Server, fe_proxy, be_register, [node()])
	  end,
    call_servers(Fun).

dispatcher_reload() ->
    Conf = ets:tab2list(e_dispatcher),

    Fun = fun(Server) ->
		  rpc:cast(Server, fe_cache, dispatcher_reload, [Conf])
	  end,
    call_servers(Fun).

invalidate(List) ->
    %% all dispacher keys are compiled for re module.
    Compiled = lists:map(fun(Regexp) ->{ok,R} = re:compile(Regexp), R end, List),
    Fun = fun(Server) ->
		  rpc:call(Server, fe_cache, invalidate_handler, [Compiled])
	  end,
    call_servers(Fun).

call_servers(Fun) ->
    FEs = e_conf:fe_servers(),
    lists:foreach(Fun, FEs).
