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
%%% @doc Module responsible for managing the backend nodes of the Erlang Web cluster.
%%% All the functions should be called from the backend server.
%%% @end
%%%-------------------------------------------------------------------
-module(e_cluster).
-behaviour(gen_server).

-include_lib("eptic/include/e_annotation.hrl").

%% ANNOTATIONS CALLBACKS
-export([invalidate/4, invalidate_if/4]).
-export([invalidate_groups/4, invalidate_groups_if/4]).
-export([backend_call/4]).

%% GEN_SERVER CALLBACKS
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).

%% API
-export([dispatcher_reload/0, invalidate/1]).
-export([be_request/4, synchronize_docroot/1, synchronize_docroot0/2]).
-export([invalidate_groups/1]).
-export([connect_to_fe0/2]).

%% 1 MB
-define(MAX_FILE_CHUNK, 1048576).

-record(state, {fe_servers, 
		ping_timeout,
		workers}).

%%
%% API
%%

%%
%% @spec dispatcher_reload() -> ok
%% @doc Updates the dispatcher entries on the all frontend nodes.
%% In order to keep the dispatcher rules consistent all over the Erlang
%% Web cluster it is strongly recommended to change it only on the backend
%% (even if it is not used there) and trigger the update by calling the
%% <i>dispatcher_reload/0</i> function.
%%
-spec(dispatcher_reload/0 :: () -> ok).	     
dispatcher_reload() ->
    gen_server:cast(?MODULE, dispatcher_reload).

%%
%% @spec invalidate(Entries) -> ok
%% Entries = [Url]
%% Url = string()
%% @doc Removes the selected urls from the cache on all frontend servers.
%% <i>Url</i> is a regular expression describing the URL that should be 
%% removed from cache (as they are no longer valid, e.g. the model has
%% changed). <br/>
%% The regular expression must be in the PCRE format (described in the 
%% Erlang manual - http://www.erlang.org/doc/man/re.html).
%% 
-spec(invalidate/1 :: (list(string())) -> ok).	     
invalidate(List) ->
    gen_server:cast(?MODULE, {invalidate, List}).

%%
%% @spec invalidate_groups(Groups) -> ok
%% Groups = [Group]
%% Group = string()
%% @doc Removes all the cache contents that belongs at least to one of the <i>Group</i>.
%% <i>Group</i> must be a string which labels and categorizes the cache entry.
%% 
-spec(invalidate_groups/1 :: (list(string())) -> ok).			  
invalidate_groups(Groups) ->
    gen_server:cast(?MODULE, {invalidate_groups, Groups}).

%% @hidden
-spec(be_request/4 :: (atom(), atom(), atom(), term()) -> {term(), term()}).	     
be_request(M, F, A, Dict) ->
    e_dict:init_state(Dict),
    {e_mod_gen:controller(M, F, A), e_dict:get_state(), self()}.

%%
%% @spec synchronize_docroot(Filename :: string()) -> WorkerPid :: pid()
%% @doc Sends the file with the <i>Filename</i> to the all frontend servers.
%% Since uploaded files are stored on the backend node this function must 
%% be called after successful saving to synchronize the state on all nodes.<br/>
%% The path to the file should be relative to the server root.
%% @see e_conf:server_root/0
%%
-spec(synchronize_docroot/1 :: (string()) -> pid()).
synchronize_docroot(Filename0) ->
    Filename = case lists:prefix(e_conf:server_root(), Filename0) of
		   true ->
		       case lists:subtract(Filename0, e_conf:server_root()) of
			   [$/ | Rest] ->
			       Rest;
			   Else ->
			       Else
		       end;
		   false ->
		       Filename0
	       end,

    spawn(?MODULE, synchronize_docroot0, [Filename]).

%%
%% GEN_SERVER CALLBACK FUNCTIONS
%%

%% @hidden
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @hidden
init(_) ->
    case application:get_env(eptic, node_type) of
	{ok, backend} ->
	    FeServers = e_conf:fe_servers(),
	    PingTmout = e_conf:get_conf(fe_ping_timeout, 1000),

	    process_flag(trap_exit, true),
	    
	    Pids = [connect_to_fe(Server, PingTmout) || Server <- FeServers],
	    
	    {ok, #state{fe_servers = FeServers,
			ping_timeout = PingTmout,
			workers = lists:zip(Pids, FeServers)}};
	_ ->
	    {ok, #state{fe_servers = [node()],
			workers = []}}
    end.

%% @hidden
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


%% @hidden
handle_cast({fe_node_up, Node, Worker}, State) ->
    case catch erlang:monitor_node(Node, true) of
	{'EXIT', _} ->
	    Pid = connect_to_fe(Node, State#state.ping_timeout),
	    {noreply, State#state{workers = [{Pid, Node} | lists:keydelete(Worker, 1, State#state.workers)]}};
	true ->
	    timer:send_after(1000, {e_fe_proxy, Node}, {be, node()}),
	    {noreply, State#state{workers = lists:keydelete(Worker, 1, State#state.workers)}}
    end;

handle_cast(dispatcher_reload, State) ->
    Conf = ets:tab2list(e_dispatcher),

    Fun = fun(Server) ->
		  rpc:cast(Server, e_fe_cache, dispatcher_reload, [Conf])
	  end,
    lists:foreach(Fun, State#state.fe_servers),

    {noreply, State};

handle_cast({invalidate, List}, State) ->
    Compiled = lists:map(fun(Regexp) ->
				 {ok, R} = re:compile(Regexp),
				 R
			 end, List),
    Fun = fun(Server) ->
		  {e_fe_cache, Server} ! {invalidate, Compiled}
	  end,
    lists:foreach(Fun, State#state.fe_servers),

    {noreply, State};

handle_cast({invalidate_groups, Groups}, State) ->
    Fun = fun(Server) ->
		  {e_fe_cache, Server} ! {invalidate_groups, Groups}
	  end,
    lists:foreach(Fun, State#state.fe_servers),
    
    {noreply, State};

handle_cast({synchronize_docroot, Filename0}, State) ->
    Filename = case lists:prefix(e_conf:server_root(), Filename0) of
		   true ->
		       case lists:subtract(Filename0, e_conf:server_root()) of
			   [$/ | Rest] ->
			       Rest;
			   Else ->
			       Else
		       end;
		   false ->
		       Filename0
	       end,
    
    spawn(?MODULE, synchronize_docroot0, [Filename, State#state.fe_servers]),

    {noreply, State};

handle_cast(Else, State) ->
    error_logger:info_msg("~p module, unknown cast: ~p~nstate: ~p~n~n", 
			  [?MODULE, Else, State]),
    
    {noreply, State}.


%% @hidden
handle_info({nodedown, Node}, State) ->
    connect_to_fe(Node, State#state.ping_timeout),

    {noreply, State};

handle_info({'EXIT', From, normal}, State) ->
    {noreply, State#state{workers = proplists:delete(From, State#state.workers)}};

handle_info({'EXIT', From, _}, State) ->
    Node = proplists:get_value(From, State#state.workers),
    Pid = connect_to_fe(Node, State#state.ping_timeout),
    
    {noreply, State#state{workers = [{Pid, Node} | lists:keydelete(From, 1, State#state.workers)]}}.


%% @hidden
terminate(_, State) ->
    lists:foreach(fun({Pid, _}) ->
			  exit(Pid, {terminate, ?MODULE})
		  end, State#state.workers).


%% @hidden
code_change(_, State, _) ->
    {ok, State}.

%%
%% HELPER FUNCTIONS
%%
-spec(connect_to_fe/2 :: (atom(), integer()) -> pid()).	     
connect_to_fe(Node, Timeout) ->
    spawn_link(?MODULE, connect_to_fe0, [Node, Timeout]).

%% @hidden
-spec(connect_to_fe0/2 :: (atom(), integer()) -> any()).	     
connect_to_fe0(Node, Timeout) ->
    receive
	after Timeout ->
		ok
	end,

    case net_adm:ping(Node) of
	pong ->
	    gen_server:cast(?MODULE, {fe_node_up, Node, self()});
	pang ->
	    connect_to_fe0(Node, Timeout)
    end.

%% @hidden
-spec(synchronize_docroot0/2 :: (string(), list(atom())) -> ok | {error, term()}).	     
synchronize_docroot0(Filename, Servers) ->
    case e_file:get_size(Filename) of
	N when N < ?MAX_FILE_CHUNK ->
	    copy_file(Filename, Servers);
	_ ->
	    copy_file_chunk(Filename, Servers)
    end.

-spec(copy_file/2 :: (string(), list(atom())) -> ok | {error, term()}).	     
copy_file(Filename, Servers) ->
    case file:read_file(Filename) of
	{ok, Binary} ->
	    Fun = fun(Server) ->
			  rpc:cast(Server, e_fe_cluster, write_file, [Filename, Binary])
		  end,
	    lists:foreach(Fun, Servers);
	{error, Reason} ->
	    {error, Reason}
    end.
    
-spec(copy_file_chunk/2 :: (string(), list(atom())) -> ok | {error, term()}).	     
copy_file_chunk(Filename, Servers) ->
    case file:open(Filename, [read, raw, binary]) of
	{ok, Fd} ->
	    copy_file_chunk(Fd, Filename, Servers),
	    
	    file:close(Fd);
	{error, Reason} ->
	    error_logger:error_msg("~p module, could not open ~p file, reason: ~p~n", 
				   [?MODULE, Filename, Reason])
    end.

-spec(copy_file_chunk/3 :: (term(), string(), list(atom())) -> ok).	     
copy_file_chunk(Fd, Filename, Servers) ->
    case file:read(Fd, ?MAX_FILE_CHUNK) of
	{ok, Data} ->
	    Fun = fun(Server) ->
			  rpc:call(Server, e_fe_cluster, write_file_chunk, [Filename, Data])
		  end,
	    lists:foreach(Fun, Servers),
	    
	    copy_file_chunk(Fd, Filename, Servers);
	eof ->
	    ok;
	{error, Reason} ->
	    error_logger:error_msg("~p module, error during reading the file ~p, reason: ~p~n",
				   [?MODULE, Filename, Reason])
    end.

%%
%% ANNOTATIONS
%% 	
?AFTER.
invalidate(Regexps, _Mod, _Fun, FunResult) ->
    case application:get_env(eptic, node_type) of
	{ok, NodeType} when NodeType == backend;
			    NodeType == single_node_with_cache ->
	    e_cluster:invalidate(Regexps);
	_ ->
	    ok
    end,
    {proceed, FunResult}.

?AFTER.
invalidate_if({Regexps, {CMod, CFun}}, _Mod, _Fun, FunResult) ->
    case CMod:CFun(FunResult) of
	true ->
	    case application:get_env(eptic, node_type) of
		{ok, NodeType} when NodeType == backend;
				    NodeType == single_node_with_cache ->
		    e_cluster:invalidate(Regexps);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    {proceed, FunResult};
invalidate_if({Regexp, CFun}, Mod, Fun, FunResult) when is_atom(CFun) ->
    invalidate_if({Regexp, {Mod, CFun}}, Mod, Fun, FunResult).

?AFTER.
invalidate_groups(Groups, _Mod, _Fun, FunResult) ->
    case application:get_env(eptic, node_type) of
	{ok, NodeType} when NodeType == backend;
			    NodeType == single_node_with_cache ->
	    e_cluster:invalidate_groups(Groups);
	_ ->
	    ok
    end,
    {proceed, FunResult}.

?AFTER.
invalidate_groups_if({Groups, {CMod, CFun}}, _Mod, _Fun, FunResult) ->
    case CMod:CFun(FunResult) of
	true ->
	    case application:get_env(eptic, node_type) of
		{ok, NodeType} when NodeType == backend;
			    NodeType == single_node_with_cache ->
		    e_cluster:invalidate_groups(Groups);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    {proceed, FunResult};
invalidate_groups_if({Groups, CFun}, Mod, Fun, FunResult) when is_atom(CFun) ->
    invalidate_groups_if({Groups, {Mod, CFun}}, Mod, Fun, FunResult).

?BEFORE.
backend_call(_, Mod, Fun, FunArgs) ->
    case application:get_env(eptic, node_type) of
	{ok, frontend} ->
	    {ok, NodeName} = application:get_env(eptic_fe, be_server_name),
	    case rpc:call(NodeName, Mod, Fun, FunArgs) of
		{badrpc, Reason} ->
		    error_logger:error_msg("~p module, error during RPC call to backend node ~p "
					   "in backend_call annotation, reason: ~p~n~n",
					   [?MODULE, NodeName, Reason]),
		    {skip, {badrpc, Reason}};
		Result ->
		    {skip, Result}
	    end;
	_ ->
	    {proceed, FunArgs}
    end.
