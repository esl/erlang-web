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
%%% File    : e_component.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%%
%%% @doc Ecomponent behaviour module.
%%% <p>The ecomponent manager is built on top of gen_server behaviour.
%%% It allows users to install and uninstall the new optional components 
%%% into their applications. </p>
%%% <p>In order to provide an excellent cooperation and integration, 
%%% the new Erlang Web component must export three callback functions:
%%% <ul>
%%% <li>install/1</li>
%%% <li>uninstall/1</li>
%%% <li>dependencies/0</li>
%%% </ul></p>
%%% <p>The role of the <i>install/1</i> function is to install all needed
%%% things in the new environment (such as: start gen_server and connect
%%% it to the main wparts supervisor - registered locally 
%%% under the <b>wpart</b> name, create ets tables, load tpls or similar).
%%% The <i>install/1</i> function takes one argument which is taken from 
%%% the configuration file (the syntax of the <i>ecomponent</i>
%%% option is the same as used in <i>sys.config</i>), so it is possible
%%% to parametrize created components.<br/>
%%% The <i>install/1</i> should return either <i>ok</i> in case of
%%% successful installation or <i>{error, Reason}</i> otherwise.</p>
%%% <p><i>uninstall/1</i> function does the opposite thing: removes the 
%%% selected component from the system. Component should clean up 
%%% all its resources before ending the uninstallation process.<br/>
%%% The returning value is the same as in <i>install/1</i>.</p>
%%% <p><i>dependencies/0</i> should return a list of component required 
%%% components (we can imagine that component using Google Maps needs 
%%% the component Google Engine at least in version 1.3) with their versions.</p>
%%% <p>Callback functions specification:
%%% ```install(Conf :: list(term())) -> ok | {error, Reason}
%%% 
%%% uninstall(Conf :: list(term())) -> ok | {error, Reason}
%%% 
%%% dependencies() -> [dependency_spec()]
%%%   dependency_spec() = {ComponentName :: atom(), {LowestVsn :: version(), HighestVsn :: version() 
%%%   version() = float() | undefined'''</p>
%%% <p>The ecomponent manager could detect dependency loops, lack of modules
%%% and version incompatibility.</p>
%%% 
%%% @todo Add the uninstall functions.
%%% @todo Add configuration passing to the depended components.
%%% @end
%%%-------------------------------------------------------------------
-module(e_component).

-behaviour(gen_server).

-export([behaviour_info/1]).

-export([install/0, install/1]).
-export([list/0]).
-export([uninstall/0, uninstall/1]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {running}).

-define(TIMEOUT, 1000).

%% @hidden
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 
%% @spec install() -> InstallationResult :: list({atom, Result})
%% Result = ok | {error, Reason}
%% Reason = term()
%% @doc Installs all ecomponents declared inside the <i>project.conf</i> file.
%% The components are installed sequentially, in order of their declaration.
%% @see e_conf:ecomponents/0
%%
-spec(install/0 :: () -> list({atom(), term()})).	     
install() ->
    Apps = application:which_applications(),
    case required_apps_started(Apps) of
	true ->
	    Ecomponents = e_conf:ecomponents(),
	    lists:zip(lists:map(fun({Name, _}) ->
					Name
				end, Ecomponents),
		      lists:map(fun install/1, Ecomponents));
	false ->
	    timer:apply_after(?TIMEOUT, ?MODULE, install, [])
    end.

%%
%% @spec install({EComponentName :: atom(), Conf :: list()}) -> ok | {error, Reason :: term()}
%% @doc Installs the selected ecomponent and all its dependencies.
%% If installation fails, the <i>{error, Reason}</i> is returned.
%%
-spec(install/1 :: ({atom(), list()}) -> ok | {error, term()}).	     
install({Name, Conf}) ->
    gen_server:call(?MODULE, {install, Name, Conf}).

%%
%% @spec list() -> InstalledComponents :: list({atom(), string(), string()})
%% @doc Returns a list of the installed components with their versions.
%% The format of the returning data is the same as in the application:which_applications/0.
%%
-spec(list/0 :: () -> list({atom(), string(), string()})).
list() ->	     
    gen_server:call(?MODULE, list).

%%
%% @spec uninstall() -> UninstallationResult :: list({ComponentName :: atom(), Result :: term()})
%% @doc Uninstalls all ecomponents declared in the <i>project.conf</i> file from the system.
%% After successful uninstallation all dependencies are removed too.
%% @see e_conf:ecomponents/0
%%
-spec(uninstall/0 :: () -> list({atom(), term()})).	     
uninstall() ->
    Ecomponents = e_conf:ecomponents(),
    lists:zip(lists:map(fun({Name, _}) ->
				Name
			end, Ecomponents),
	      lists:map(fun uninstall/1, Ecomponents)).

%%
%% @spec uninstall({Name :: atom(), Configuration :: list()}) -> ok | {error, Reason :: term()}
%% @doc Uninstalls specified component from the system.
%% It is possible to pass some configuration details to the component
%% so it knows how it should be removed.
%%
-spec(uninstall/1 :: ({atom(), list()}) -> term()).
uninstall({Name, Conf}) ->
    gen_server:call(?MODULE, {uninstall, Name, Conf}).
    
%% @hidden
init([]) ->
    install(),
    {ok, #state{running = []}}.

%% @hidden
handle_call({install, Name, Conf}, _From, State) ->
    {Reply, NewState} = run_ecomponent(Name, latest, Conf, [], State),
    {reply, Reply, NewState};
handle_call(list, _, #state{running = Running} = State) ->
    {reply, Running, State};
handle_call({uninstall, Name, Conf}, _From, State) ->
    {Reply, NewState} = stop_ecomponent(Name, Conf, State),
    {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @hidden 
-spec(behaviour_info/1 :: (atom()) -> list(tuple()) | undefined).	     
behaviour_info(callbacks) ->
    [{install, 1},
     {uninstall, 1},
     {dependencies, 0}];
behaviour_info(_) ->
    undefined.

-spec(run_ecomponent/5 :: (atom(), latest | number(), list(tuple()), list(atom()), tuple()) -> {term(), tuple()}).	     
run_ecomponent(Name, latest, Conf, CallStack, State) ->
    case lists:keymember(Name, 1, State#state.running) of
	true ->
	    {ok, State};
	false ->
	    case check_dependency_loop(Name, CallStack) of
		true ->
		    {{error, {dependency_loop, [Name | CallStack]}}, State};
		false ->
		    install_ecomponent(Name, Conf, [Name | CallStack], State)
	    end
    end;
run_ecomponent(Name, Vsn, Conf, CallStack, State) ->
    case lists:keysearch(Name, 1, State#state.running) of
	{_, {_, RVsn}} ->
	    case check_running_version(RVsn, Vsn) of
		true ->
		    {ok, State};
		false ->
		    {{error, {no_version_match, Vsn}}, State}
	    end;
	false ->
	    case check_dependency_loop(Name, CallStack) of
		true ->
		    {{error, {dependency_loop, [Name | CallStack]}}, State};
		false ->
		    {Ret, NewState} = install_ecomponent(Name, Conf, [Name | CallStack], State),
		    case lists:keysearch(Name, 1, NewState#state.running) of
			{_, {_, IVsn}} ->
			    case check_running_version(IVsn, Vsn) of
				true ->
				    {Ret, NewState};
				false ->
				    error_logger:error_msg("~p module, no version match for ~p (~p vsn)~n",
							   [?MODULE, Name, Vsn]),
				    
				    {{error, {no_version_match, Vsn}}, NewState}
			    end;
			false ->
			    {Ret, NewState}
		    end
	    end
    end.
	    
-spec(check_dependency_loop/2 :: (atom(), list(atom())) -> bool()).	     
check_dependency_loop(Name, CallStack) ->
    case lists:member(Name, CallStack) of
	true ->
	    error_logger:error_msg("~p module, entered the dependecy conflict!~n"
				   "The dependency loop: ~p~n",
				   [?MODULE,  lists:reverse([Name | CallStack])]),

	    true;
	false ->
	    false
    end.

-type(version() :: number() | undefined).
-spec(check_running_version/2 :: (integer(), {version(), version()}) -> bool()).	     
check_running_version(_, {undefined, undefined}) ->
    true;
check_running_version(Vsn, {LVsn, undefined}) ->
    Vsn >= LVsn;
check_running_version(Vsn, {undefined, UVsn}) ->
    Vsn =< UVsn;
check_running_version(Vsn, {LVsn, UVsn}) ->
    Vsn >= LVsn andalso Vsn =< UVsn.

-spec(install_ecomponent/4 :: (atom(), list(tuple()), list(atom()), tuple()) -> {term(), tuple()}).	     
install_ecomponent(Name, Conf, CallStack, State) ->
    case check_dependencies(Name, State#state.running) of
	{'EXIT', _} ->
	    error_logger:error_msg("~p module, no ecomponent found: ~p~n", [?MODULE, Name]),
	    
	    {{error, {ecomponent_does_not_exist, Name}}, State};
	{error, {no_version_match, DName, DVsn}} = Error ->
	    error_logger:error_msg("~p module, no version match for version ~p of ~p~n"
				   "It is a dependency of ~p~n",
				   [?MODULE, DVsn, DName, Name]),
	    
	    {Error, State};
	[] ->
	    transactional_start(Name, Conf, State);
	Deps ->
	    {Result, NewState} = run_deps(Deps, CallStack, State),
	    case no_errors(Result, []) of
		true ->
		    transactional_start(Name, Conf, NewState);
		Errors ->
		    {{error, {can_not_run_dependencies, Errors}}, NewState}
	    end
    end.

-spec(check_dependencies/2 :: (atom(), list(tuple())) -> tuple() | list(tuple())).	     
check_dependencies(Name, Running) ->
    case catch Name:dependencies() of
	Deps when is_list(Deps) ->
	    Result = lists:map(fun({DName, Vsn}) ->
				       case lists:keysearch(DName, 1, Running) of
					   {_, {_, RVsn}} ->
					       case check_running_version(RVsn, Vsn) of
						   true ->
						       {running, DName};
						   false ->
						       {error, {no_version_match, DName, Vsn}}
					       end;
					   false ->
					       {not_running, {DName, Vsn}}
				       end
			       end, Deps),
	    case lists:keysearch(error, 1, Result) of
		{_, Error} ->
		    Error;
		false ->
		    NotRunning = lists:filter(fun({not_running, _}) ->
						      true;
						 (_) ->
						      false
					      end, Result),
		    lists:map(fun({_, Desc}) -> Desc end, NotRunning)
	    end;
	Else ->
	    Else
    end.

-spec(run_deps/3 :: (list(tuple()), list(atom()), tuple()) -> {list(term()), tuple()}).	     
run_deps(Deps, CallStack, State) ->
    lists:foldl(fun({Name, Vsn}, {Statuses, OldState}) ->
			{Status, NewState} = run_ecomponent(Name, Vsn, [], CallStack, OldState),
			{[Status | Statuses], NewState}
		end, {[], State}, Deps).
	
-spec(get_vsn/1 :: (atom()) -> float()).	     
get_vsn(Name) ->
    case lists:keysearch(Name, 1, application:which_applications()) of
	{_, {_, _, Vsn}} ->
	    list_to_float(Vsn);
	false ->
	    0
    end.

-spec(no_errors/2 :: (list(ok | tuple()), list(tuple())) -> true | list(tuple())).	     
no_errors([], []) ->
    true;
no_errors([], Errors) ->
    Errors;
no_errors([ok | Rest], Errors) ->
    no_errors(Rest, Errors);
no_errors([Error | Rest], Errors) ->
    no_errors(Rest, [Error | Errors]).

-spec(required_apps_started/1 :: (list()) -> bool()).
required_apps_started(Apps) ->
    lists:all(fun(App) ->
		      lists:keymember(App, 1, Apps)
	      end, [eptic, wpart, wparts]).

-spec(stop_ecomponent/3 :: (atom(), list(), tuple()) -> ok | {error, term()}).	     
stop_ecomponent(Name, Conf, State) ->
    case lists:keymember(Name, 1, State#state.running) of
	true ->
	    transactional_stop(Name, Conf, State);
	false ->
	    {{error, {not_running, Name}}, State}
    end.

-spec(transactional_start/3 :: (atom(), list(), tuple()) -> {term(), tuple()}).	     
transactional_start(Name, Conf, State) ->
    case catch Name:install(Conf) of
	ok ->
	    case application:start(Name) of
		ok ->
		    Vsn = get_vsn(Name),
		    
		    {ok, State#state{running = [{Name, Vsn} | State#state.running]}};
		AppElse ->
		    case catch Name:uninstall(Conf) of
			ok ->
			    error_logger:error_msg("~p module, could not install a component ~p~n"
						   "skipping it~n", [?MODULE, Name]),
			    
			    {AppElse, State};
			UnElse ->
			    erlang:error({error, {installation_process_failed, UnElse}})
		    end
	    end;
	Else ->
	    {Else, State}
    end.

transactional_stop(Name, Conf, State) ->
    case application:stop(Name) of
	ok ->
	    case catch Name:uninstall(Conf) of
		ok ->
		    {ok, State#state{running = proplists:delete(Name, State#state.running)}};
		UnElse ->
		    case application:start(Name) of
			ok ->
			    error_logger:error_msg("~p module, could not uninstall a component ~p~n"
						   "skipping it~n", [?MODULE, Name]),
			    
			    {UnElse, State};
			AppElse ->
			    erlang:error({error, {uninstallation_process_failed, AppElse}})
		    end
	    end;
	Else ->
	    {Else, State}
    end.
