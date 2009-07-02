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
%% Ltd. Portions created by Erlang Training & Consulting Ltd are Copyright 2009,
%% Erlang Training & Consulting Ltd. All Rights Reserved.

%%%-------------------------------------------------------------------
%%% File    : e_hook.erl
%%% Author  : michalptaszek <michalptaszek@paulgray>
%%% Description : Hook engine implementation for Erlang Web framework.
%%%
%%% Created :  1 Jul 2009 by michalptaszek <michalptaszek@paulgray>
%%%-------------------------------------------------------------------
-module(e_hook).

-behaviour(gen_server).

%% API
-export([start_link/0, add/3, delete/2, run/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(hook, {name, callbacks}).

-spec(add/3 :: (atom(), function(), integer()) -> ok).
add(HookPoint, Function, Priority) ->
    gen_server:cast(?MODULE, {add, HookPoint, Function, Priority}).

-spec(delete/2 :: (atom(), function()) -> ok).
delete(HookPoint, Function) ->
    gen_server:cast(?MODULE, {delete, HookPoint, Function}).

-spec(run/2 :: (atom(), term()) -> term()).
run(HookPoint, StartArgs) ->
    case ets:lookup(?MODULE, HookPoint) of
	[#hook{callbacks = Funs}] ->
	    run_hooks(Funs, StartArgs);
	[] ->
	    StartArgs
    end.
    
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ets:new(?MODULE, [named_table, public, 
		      {keypos, #hook.name}]),
    
    {ok, ok}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%
%% Add new hook into the callbacks list.
%% If there is no callback defined new entry will be created.
%%
handle_cast({add, Name, Fun, Priority}, State) ->
    case ets:lookup(?MODULE, Name) of
	[] ->
	    ets:insert(?MODULE, #hook{name = Name,
				      callbacks = [{Priority, Fun}]});
	[Hook] ->
	    ets:insert(?MODULE, 
		       Hook#hook{callbacks = insert_callback(Priority,
							     Fun,
							     Hook#hook.callbacks)})
    end,

    {noreply, State};

%%
%% Remove the function from the callback lists on the specified hook point.
%% If it is the only function on the hook list, the whole hook point will
%% be removed.
%% 
handle_cast({delete, Name, Fun}, State) ->
    case ets:lookup(?MODULE, Name) of
	[] ->
	    ok;
	[Hook] ->
	    case lists:keydelete(Fun, 2, Hook#hook.callbacks) of
		[] ->
		    ets:delete(?MODULE, Name);
		Rest ->
		    ets:insert(?MODULE, Hook#hook{callbacks = Rest})
	    end
    end,

    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec(insert_callback/3 :: (integer(), function(), list()) -> list()).
insert_callback(Priority, Fun, Hooks) ->
    lists:merge(Hooks, [{Priority, Fun}]).

-spec(run_hooks/2 :: (list(), term()) -> term()).
run_hooks([{_, Fun} | Rest], Args) ->
    case Fun(Args) of
	{ok, NewArgs} ->
	    run_hooks(Rest, NewArgs);
	{break, NewArgs} ->
	    NewArgs
    end;
run_hooks([], Args) ->
    Args.
