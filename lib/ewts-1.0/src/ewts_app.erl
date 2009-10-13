%%%-------------------------------------------------------------------
%%% File    : ewts_app.erl
%%% Author  : michalptaszek <michalptaszek@paulgray>
%%% Description : Main application of Erlang Web Testing Suite.
%%%
%%% Created : 16 Jun 2009 by michalptaszek <michalptaszek@paulgray>
%%%-------------------------------------------------------------------
-module(ewts_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
-spec(start/2 :: (term(), list()) -> 
	     {ok, pid()} | {ok, pid(), term()} | {error, term()}).	     
start(_Type, StartArgs) ->
    ewts_sup:start_link(StartArgs).

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
-spec(stop/1 :: (term()) -> ok).	     
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
