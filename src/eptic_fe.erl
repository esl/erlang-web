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
%%% File    : eptic_fe.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Main module for frontend Erlang Web application.
%%% <p>It starts the separate application and the main supervisor of the
%%% frontend node. </p>
%%% <p>Frontend application has access only to the session linked with
%%% the passed cookie and to the arguments server uses (cookies, headers, etc).</p>
%%% <p>The main role for frontend is to be lightweight node for 
%%% caching the content returned from the backend.</p>
%%% @end
%%%-------------------------------------------------------------------
-module(eptic_fe).

-behaviour(supervisor).
-behaviour(application).

%% API
-export([start_link/0]).
-export([load_conf/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% @hidden
start(_, _) ->
    load_conf(),
    supervisor:start_link(eptic_fe, []).

%% @hidden
stop(_) ->
    ok.

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    load_conf(),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    Proxy = {e_fe_proxy, {e_fe_proxy, start_link, []},
	     permanent, 1000, worker, dynamic},

    Cache = {e_fe_cache, {e_fe_cache, start_link, []},
	     permanent, 2000, worker, dynamic},

    GC = {e_fe_gc, {e_fe_gc, start_link, []},
	  permanent, 10000, worker, dynamic},

    {ok,{{one_for_one, 1, 5}, [Proxy, Cache, GC]}}.

%%====================================================================
%% Internal functions
%%====================================================================
load_conf() ->
    ets:new(e_fe_conf, [public, named_table]),

    case file:consult("config/project_fe.conf") of
	{ok, Tuples} ->
	    ets:insert(e_fe_conf, Tuples);
	{error, Reason} ->
	    error_logger:error_msg("Error during read configuration file config/frontend.conf, " 
				   "reason: ~p", [Reason])
    end.
