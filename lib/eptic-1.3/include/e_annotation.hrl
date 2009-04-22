%%
%% Helper header that allows using macros like they were annotations.
%% The basic usage is to hide the complexity of the calls to user,
%% so in the future it will be totally transparent: if we are
%% running our service on the single node or using the cluster of
%% Erlang VMs.
%% Each of the macros should be placed above the function they will
%% be related to. It is possible to place more than one macro above 
%% the one function.
%%

%%
%% Invalidates the content on the frontend servers.
%%
%% The following function:
%%
%% ?INVALIDATE(["^/blog/", "^/index\.html$"]).
%% create_new_post(Content) ->
%%     wtype_post:create(Content),
%%     mailing_list:announce(new_post, Content),
%%    
%%     {redirect, "/index.html"}.
%%
%% will be transformed into:
%%
%% create_new_post(Content) ->
%%     Result = begin
%% 		 wtype_post:create(Content),
%% 		 mailing_list:announce(new_post, Content),
%%    
%% 		 {redirect, "/index.html"}
%% 	        end,
%%
%%     case application:get_env(eptic, node_type) of
%% 	{ok, NodeType} when NodeType == backend; 
%% 			    NodeType == single_node_with_cache ->
%% 	    e_cluster:invalidate(["^/blog/", "^/index\.html$"]);
%% 	_ ->
%% 	    ok
%%     end,
%%     Result.
-define(INVALIDATE(Regexps), 
	-invalidate(Regexps)).

%%
%% Invalidates the content on the frontend servers only when
%% the evaluation of the Pred function (could be either a local call
%% - then the Pred is an atom or a remote call - then the Pred
%% is a tuple of atoms: {Mod, Fun}) that takes a one argument - 
%% a result of the actual function evaluation - returns true.
%% Otherwise, the invalidation process is not performed.
%%
%% The following function:
%%
%% ?INVALIDATE_IF(["^/blog/", "^/index\.html$"], checker).
%% create_new_post(Content) ->
%%     wtype_post:create(Content),
%%     case mailing_list:announce(new_post, Content) of
%% 	   ok ->
%% 	       {redirect, "/index.html"};
%% 	   {error, Reason} ->
%% 	       wpart:fset("error_msg", Reason),
%% 	       {template, "error/mailing_list_error.html"}
%%        end.

%% checker({redirect, _}) ->
%%     true;
%% checker(_) ->
%%     false.

%%
%% will be transformed into:
%%
%% create_new_post(Content) ->
%%     Result = begin
%% 		 wtype_post:create(Content),
%% 		 mailing_list:announce(new_post, Content),
%%    
%% 		 {redirect, "/index.html"}
%% 	        end,
%%
%%      case checker(Result) of
%% 	 true ->
%% 	     case application:get_env(eptic, node_type) of
%% 		 {ok, NodeType} when NodeType == backend; 
%% 				     NodeType == single_node_with_cache ->
%% 		     e_cluster:invalidate(["^/blog/", "^/index\.html$"]);
%% 		 _ ->
%% 		     ok
%% 	     end,
%% 	     Result;
%% 	 _ ->
%% 	     ok
%%      end,
%%      Result.
-define(INVALIDATE_IF(Regexps, Pred),
	-invalidate(Regexps, Pred)).

%%
%% Those two macros does the same as the ?INVALIDATE and ?INVALIDATE_IF
%% but operates on groups (e_cluster:invalidate_groups/1 is called).
%% 
-define(INVALIDATE_GROUPS(Groups),
	-invalidate_groups(Groups)).

-define(INVALIDATE_GROUPS_IF(Groups, Pred),
	-invalidate_groups(Groups, Pred)).

%%
%% Performs the action on the backend node.
%% 
%%
%% The following function:
%%
%% ?BACKEND_CALL.
%% read_all_posts() ->
%%     e_db:read(posts).
%%
%% will be transformed into:
%%
%% read_all_posts(Args) ->
%%     case application:get_env(eptic, node_type) of
%% 	{ok, frontend} ->
%% 	    {ok, NodeName} = application:get_env(eptic_fe, be_server_name),
%% 	    case rpc:call(NodeName, ?MODULE, read_all_posts, [Args]) of
%% 		{badrpc, Reason} ->
%% 		    error_logger:error_msg("~p module, error during RPC call to backend, reason: ~p~n",
%% 					   [?MODULE, Reason]),
%% 		    {badrpc, Reason};
%% 		Result ->
%% 		    Result
%% 	    end;
%% 	_ ->
%% 	    e_db:read(posts)
%%     end.
-define(BACKEND_CALL,
	-backend_call([])).






-compile({parse_transform, e_annotation2}).

-define(BEFORE,
	-ew_annotation_before([])).

-define(AFTER,
	-ew_annotation_after([])).
