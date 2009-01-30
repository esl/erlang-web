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

-export([inform_fe_servers/0, dispatcher_reload/0, invalidate/1]).
-export([be_request/4, synchronize_docroot/1, synchronize_docroot0/1]).

%% 1 MB
-define(MAX_FILE_CHUNK, 1 bsl 20).

%%
%% @spec inform_fe_servers() -> ok
%% @doc Announces the presence of the backend server to the all known frontends.
%% The informing backend server becomes the main one, which all the needed
%% requests will be forwarded to. <br/>
%% The list of the frontend servers is retrieved from the <i>project.conf</i>
%% file.
%% @see e_conf:fe_servers/0
%%
-spec(inform_fe_servers/0 :: () -> ok).	     
inform_fe_servers() ->
    Fun = fun(Server) ->
		  rpc:cast(Server, e_fe_proxy, be_register, [node()])
	  end,
    call_servers(Fun).

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
    Conf = ets:tab2list(e_dispatcher),

    Fun = fun(Server) ->
		  rpc:cast(Server, e_fe_cache, dispatcher_reload, [Conf])
	  end,
    call_servers(Fun).

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
    Compiled = lists:map(fun(Regexp) ->
				 {ok, R} = re:compile(Regexp), 
				 R 
			 end, List),
    Fun = fun(Server) ->
		  rpc:call(Server, e_fe_cache, invalidate_handler, [Compiled])
	  end,
    call_servers(Fun).

%% @hidden
-spec(be_request/4 :: (atom(), atom(), atom(), term()) -> {term(), term()}).	     
be_request(M, F, A, Dict) ->
    e_dict:init_state(Dict),
    {e_mod_gen:controller(M, F, A), e_dict:get_state(), self()}.

-spec(call_servers/1 :: (fun()) -> ok).	     
call_servers(Fun) ->
    FEs = e_conf:fe_servers(),
    lists:foreach(Fun, FEs).

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

%% @hidden
-spec(synchronize_docroot0/1 :: (string()) -> ok | {error, term()}).	     
synchronize_docroot0(Filename) ->
    case e_file:get_size(Filename) of
	N when N < ?MAX_FILE_CHUNK ->
	    copy_file(Filename);
	Size ->
	    copy_file_chunk(Filename, Size)
    end.

-spec(copy_file/1 :: (string()) -> ok | {error, term()}).	     
copy_file(Filename) ->
    case file:read_file(Filename) of
	{ok, Binary} ->
	    Fun = fun(Server) ->
			  rpc:cast(Server, e_fe_cluster, write_file, [Filename, Binary])
		  end,
	    call_servers(Fun);
	{error, Reason} ->
	    {error, Reason}
    end.
    
-spec(copy_file_chunk/2 :: (string(), integer()) -> ok | {error, term()}).	     
copy_file_chunk(Filename, Size) ->
    ok.
