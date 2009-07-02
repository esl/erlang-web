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
%%% File    : eptic.erl
%%% @author Martin Carlson <martin@erlang-consulting.com>
%%% @doc API for all dictionary, cache and template functions.
%%% @end
%%%-------------------------------------------------------------------
-module(eptic).
-behaviour(application).
-behaviour(supervisor).

%% API
-export([start/2, stop/1, reload/0]).
-export([init/1]).

%% e_dict
-export([fget/1, fget/2, fget/3, fset/2, fset/3, finsert/3]).
-export([fdelete/1, fdelete/2]).
%% e_cache
-export([read_file/1]).

%%====================================================================
%% API for application
%%====================================================================
%% @hidden
start(_, _) ->
    start_link().

%% @hidden
stop(_) ->
    ok.

%%
%% @spec reload() -> ok
%% @doc Reloads the configuration and compiles all the changed files.
%%
-spec(reload/0 :: () -> ok).	     
reload() ->
    make:all([load]),
    e_dispatcher:reinstall(),
    e_conf:reinstall().

%%====================================================================
%% API for s_dict
%%====================================================================
%% @see e_dict:fget/1
-spec(fget/1 :: (term()) -> term()).	     
fget(Key) ->
    e_dict:fget(Key).
    
%% @see e_dict:fget/2
-spec(fget/2 :: (term(), term()) -> term()).	     
fget(List, Key) ->
    e_dict:fget(List, Key).

%% @see e_dict:fget/3
-spec(fget/3 :: (term(), term(), fun()) -> term()).
fget(List, Key, Validator) ->
    e_dict:fget(List, Key, Validator).

%% @see e_dict:fset/2
-spec(fset/2 :: (term(), term()) -> true).	     
fset(Key, Value) ->
    e_dict:fset(Key, Value).

%% @see e_dict:fset/3
-spec(fset/3 :: (term(), term(), term()) -> true).	     
fset(List, Key, Value) ->
    e_dict:fset(List, Key, Value).

%% @see e_dict:finsert/3
-spec(finsert/3 :: (term(), term(), term()) -> true).	     
finsert(List, Key, Value) ->
    e_dict:finsert(List, Key, Value).

%% @see e_dict:fdelete/1
fdelete(Key) ->
    e_dict:fdelete(Key).

%% @see e_dict:fdelete/2
fdelete(List, Key) ->
    e_dict:fdelete(List, Key).

%%====================================================================
%% API for s_cache
%%====================================================================
%% @see e_cache:read_file/1
read_file(File) ->
    e_cache:read_file(File).

%%====================================================================
%% Internal functions
%%====================================================================
start_link() ->
    lists:foreach(fun(Mod) ->
			  Mod:install()
		  end, [e_dispatcher, e_conf, e_lang, e_component, e_cache]),

    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor functions
%%====================================================================
init([]) ->
    Dict = {e_dict, {e_dict, start_link, []},
	    permanent, 2000, worker, dynamic},
    Session = {e_session, {e_session, start_link, []},
	       permanent, 2000, worker, dynamic},
    Components = {e_component, {e_component, start_link, []},
		  permanent, 2000, worker, dynamic},
    Logger = {e_logger, {e_logger, start_link, []},
	      permanent, 2000, worker, dynamic},
    Hook = {e_hook, {e_hook, start_link, []},
	    permanent, 2000, worker, dynamic},

    List0 = [Hook, Dict, Session, Components, Logger],

    NodeType = case application:get_env(eptic, node_type) of
		   undefined ->
		       single_node;
		   {ok, T} -> 
		       T
	       end,
    List = case NodeType of
	       A when A == backend;
		      A == single_node_with_cache ->
		   e_db:install(),

		   List0 ++ [{e_cluster, {e_cluster, start_link, []},
			      permanent, 1000, worker, dynamic}];
	       single_node ->
		   e_db:install(),
		   List0;
	       frontend ->
		   List0
	   end,

    {ok, {{one_for_one, 1, 10}, List}}.

%%====================================================================
%% Internal functions
%%====================================================================
