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
-export([be_register/1, request/2, req_exec/5]).

-spec(start_link/0 :: () -> {ok, pid()}).	     
start_link() ->
    Pid = spawn_link(?MODULE, start, []),
    register(?MODULE, Pid),
    {ok, Pid}.

-spec(be_register/1 :: (atom()) -> {be, atom()}).	     
be_register(Name) ->
    e_fe_proxy ! {be, Name}.

-spec(request/2 :: (tuple(), atom()) -> term() | ok).	     
request(A, ServerMod) ->
    Session = e_fe_session:get_session(),
    e_fe_proxy ! {req, A, Session, ServerMod, self()},

    receive 
	{res, {Res, NewSession}} ->
	    e_fe_session:save_session(NewSession),
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
	    io:format("~p module, got ~p~n", [?MODULE, Name]),
	    application:set_env(eptic_fe, be_server_name, Name);
	_ -> 
	    wait_be()
    end.
 
-spec(wait_req/0 :: () -> none()).	     
wait_req() ->
    receive
	{req, A, Session, ServerMod, OutPid} -> 
	    {ok, Name} = application:get_env(eptic_fe, be_server_name),
	    spawn(e_fe_proxy, req_exec, [A, Session, ServerMod, Name, OutPid]),
	    wait_req();
	{be, Name} ->
	    application:set_env(eptic_fe, be_server_name, Name),
	    wait_req();
	_ -> 
	    wait_req()
    end.

-spec(req_exec/5 :: (tuple(), term(), atom(), atom(), pid()) -> error | {res, term()}).	     
req_exec(ServerArg, Session, ServerMod, Name, OutPid) ->
    case rpc:call(Name, ServerMod, fe_request, [A, Session]) of
	{badrpc, Error} ->
	    error_logger:error_msg("~p module, rpc error: ~p~n", [?MODULE, Error]),
	    OutPid ! error;
	Res ->
	    OutPid ! {res, Res}
    end.
