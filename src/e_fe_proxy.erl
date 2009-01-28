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
%%% File    : e_fe_proxy.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%%-------------------------------------------------------------------
-module(e_fe_proxy).
-export([start_link/0]).

-export([start/0]).
-export([be_register/1, request/3, req_exec/6]).
-export([cleanup_backend/1]).

-spec(start_link/0 :: () -> {ok, pid()}).	     
start_link() ->
    Pid = spawn_link(?MODULE, start, []),
    register(?MODULE, Pid),
    {ok, Pid}.

-spec(be_register/1 :: (atom()) -> {be, atom()}).	     
be_register(Name) ->
    e_fe_proxy ! {be, Name}.

-spec(request/3 :: (atom(), atom(), list()) -> term()).	     
request(M, F, A) ->
    {ok, Dict} = e_dict:get_state(),
    ?MODULE ! {req, M, F, A, Dict, self()},

    receive 
	{res, Res, NewDict} ->
	    e_dict:init_state(NewDict),
	    Res;
	error ->
	    ok
    end.

-spec(start/0 :: () -> none()).	     
start() ->
    wait_be(),
    wait_req().

-spec(wait_be/0 :: () -> none()).	      
wait_be() -> 
    receive 
	{be, Name} ->
	    error_logger:info_msg("~p module, got ~p~n", [?MODULE, Name]),
	    application:set_env(eptic_fe, be_server_name, Name);
	_ -> 
	    wait_be()
    end.
 
-spec(wait_req/0 :: () -> none()).	     
wait_req() ->
    receive
	{req, M, F, A, Dict, OutPid} -> 
	    {ok, Name} = application:get_env(eptic_fe, be_server_name),
	    spawn(?MODULE, req_exec, [Name, M, F, A, Dict, OutPid]);
	{be, Name} ->
	    application:set_env(eptic_fe, be_server_name, Name);
	_ -> 
	    ok
    end,
    wait_req().

-spec(req_exec/6 :: (atom(), atom(), atom(), atom(), term(), pid()) -> error | {res, term(), term()}).	     
req_exec(Name, M, F, A, Dict, OutPid) ->
    case rpc:call(Name, e_cluster, be_request, [M, F, A, Dict]) of
	{badrpc, Error} ->
	    error_logger:error_msg("~p module, rpc error: ~p~n", [?MODULE, Error]),
	    OutPid ! error;
	{Res, {ok, NewDict}, Pid} ->
	    eptic:fset("__backend_pid", Pid),
	    OutPid ! {res, Res, NewDict}
    end.

-spec(cleanup_backend/1 :: (atom()) -> none()).	     
cleanup_backend(Mod) ->
    case wpart:fget("__backend_pid") of
	undefined ->
	    ok;
	Pid ->
	    {ok, Name} = application:get_env(eptic_fe, be_server_name),
	    rpc:cast(Name, Mod, terminate, [Pid])
    end.
