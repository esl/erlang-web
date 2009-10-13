%%%-------------------------------------------------------------------
%%% File    : ewts_sup.erl
%%% Author  : michalptaszek <michalptaszek@paulgray>
%%% Description : Main supervisor of Erlang Web Testing Suite
%%%
%%% Created : 16 Jun 2009 by michalptaszek <michalptaszek@paulgray>
%%%-------------------------------------------------------------------
-module(ewts_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
-spec(start_link/1 :: (list()) -> {ok, pid()} | ignore | {error, term()}).
start_link(StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, StartArgs).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
-spec(init/1 :: (list()) -> {ok, {tuple(), list()}}).
init(_) ->
    Client = {client, {ewts_client, start_link, []},
	      permanent, 2000, worker, dynamic},
    Dbg = {dbg_tester, {ewts_dbg, start_link, []},
	   permanent, 2000, worker, dynamic},

    {ok,{{one_for_one,2,5000}, [Client, Dbg]}}.

%%====================================================================
%% Internal functions
%%====================================================================
