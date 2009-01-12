%% the contents of this file are subject to the erlang web public license,
%% version 1.0, (the "license"); you may not use this file except in
%% compliance with the license. you should have received a copy of the
%% erlang web public license along with this software. if not, it can be
%% retrieved via the world wide web at http://www.erlang-consulting.com/.
%%
%% software distributed under the license is distributed on an "as is"
%% basis, without warranty of any kind, either express or implied. see
%% the license for the specific language governing rights and limitations
%% under the license.
%%
%% the initial developer of the original code is erlang training & consulting
%% ltd. portions created by erlang training & consulting ltd are copyright 2008,
%% erlang training & consulting ltd. all rights reserved.

%%%-------------------------------------------------------------------
%%% file    : e_fe_inets.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%%-------------------------------------------------------------------
-module(e_fe_mod_inets).

-export([do/1, start/0]).

-include_lib("inets/src/httpd.hrl").

do(#mod{parsed_header = Headers} = A) ->
    case handle_args(A) of
	{ok, Args} ->
	    [$/ | URL] = A#mod.request_uri,
	    e_dict:init_state(Args),

	    case A#mod.socket_type of 
		{ssl, _} ->
		    e_dict:fset("__https", true);
		_ ->
		    e_dict:fset("__https", false)
	    end,

	    ClientCookie = e_mod_inets:cookie_up(Headers),

	    e_dict:fset("__path", URL),
	    e_dict:fset("__cookie_key", ClientCookie),
	    e_dict:fset("__cacheable", is_cacheable()),

	    ControllerFun = fun() ->
				    case e_fe_mod_gen:handle_request(URL) of
					enoent ->
					    enoent;
					{ready, Ready} ->
					    Ready;
					{not_ready, NotReady, View} ->
					    controller_exec(NotReady, View, URL)
				    end
			    end,
	    case with_formatted_error(ControllerFun) of
		{NewHeaders, Result} ->
		    CookieHeader = e_mod_inets:cookie_bind(ClientCookie),
		    e_mod_inets:cleanup(),
		    
		    {proceed, [{response, {response, [CookieHeader | NewHeaders], Result}}]};
		enoent ->
		    e_mod_inets:cookie_bind(ClientCookie),
		    e_mod_inets:cleanup(),

		    {proceed, A#mod.data}
	    end
    end.

-spec(start/0 :: () -> ok).	     
start() ->
    application:start(ssl),
    application:start(sasl),
    
    application:start(eptic),
    application:start(wpart),
    application:start(wparts),

    application:set_env(inets, services, [{httpd, filename:join(code:priv_dir(eptic_fe), "inets.conf")},
					  {httpd, filename:join(code:priv_dir(eptic_fe), "inets_https.conf")}]),
    inets:start(),

    application:start(eptic_fe),

-spec(is_cacheable/0 :: () -> bool()).	     
is_cacheable() ->
%% Here is the place where we can check, if the should skip cache
%% and redirect the request straight to the backend server.
%% 
%% We can decide wheter hit the cache or not after we inspect the
%% content of the session (eptic:fget/1).
%%
%% As an example, all requests replies are cached.
    true.
    
-spec(handle_args/1 :: (tuple()) -> {ok, list(tuple())}).	     
handle_args(#mod{method = Method, entity_body = Post} = Mod) ->
    case Method of
	"POST" ->
	    {ok, [{"get", e_mod_inets:parse_get(Mod#mod.request_uri)},
		  {"post", parse_post(Post)}]};
	_ ->
	    {ok, [{"get", e_mod_inets:parse_get(Mod#mod.request_uri)}]}
    end.

-spec(parse_post/1 :: (string()) -> list(tuple())).	     
parse_post(String) ->
    case e_mod_inets:fetch_boundary(String) of
	{simple, Data} ->
	    httpd:parse_query(Data);
	{multipart, Boundary} ->
	    {ok, Node} = application:get_env(eptic_fe, be_server_name),
	    case rpc:call(Node, e_multipart_inets, get_multipart, [String, Boundary]) of
		{badrpc, Reason} ->
		    error_logger:error_msg("~p module, error during multipart parse, reason: ~p~n",
					   [?MODULE, Reason]),
		    [];
		Else ->
		    Else
	    end
    end.

-spec(controller_exec/3 :: (term(), term(), string()) -> {term(), term()}).	     
controller_exec({ret_view, Ret, View}, _, URL) ->
    Response = controller_exec(Ret, View),
    e_fe_cache:save_cache(URL, Response),
    Response;
controller_exec({html, HTML}, View, URL) ->
    Response = controller_exec({content, html, HTML}, View),
    e_fe_cache:save_cache(URL, Response),
    Response;
controller_exec([_Status, _HTML] = Error, _, URL) ->
    e_fe_cache:save_cache(URL, Error),
    Error;
controller_exec(Else, View, URL) ->
    Response = controller_exec(Else, View),
    e_fe_cache:save_cache(URL, Response),
    Response.

-spec(controller_exec/2 :: (term(), string()) -> {term(), term()}).
controller_exec(template, View) ->
    e_mod_inets:format_response(e_fe_mod_gen:view(View));
controller_exec({redirect, URL}, _) ->
    {[{code, 302}, {location, URL}, {content_length, "0"}], []};
controller_exec({content, html, Data}, _) ->
    Content = lists:flatten(Data),
    Length = {content_length, integer_to_list(length(Content))},
    {[{content_type, "text/html"}, {code, 200}, Length], Content};
controller_exec({content, text, Data}, _) ->
    Content = lists:flatten(Data),
    Length = {content_length, integer_to_list(length(Content))},
    {[{content_type, "text/plain"}, {code, 200}, Length], Content};
controller_exec({json, Data}, _) ->
    Content = e_json:encode(Data),
    Length = {content_length, integer_to_list(length(Content))},
    {[{content_type, "text/plain"}, {code, 200}, Length], Content};
controller_exec({template, Template}, _) ->
    e_mod_inets:format_response(e_fe_mod_gen:view(Template));
controller_exec({custom, Custom}, _) ->
    Custom;
controller_exec({headers, Headers, NewRet}, View) ->
    {NewHeaders, ProperRet} = controller_exec(NewRet, View),
    ReadyHeaders = e_mod_inets:create_headers(Headers, []),
    {ReadyHeaders ++ NewHeaders, ProperRet};
controller_exec({error, Code}, _) ->
    e_mod_inets:format_response(e_mod_gen:error_page(Code, e_dict:fget("__path"))).

-spec(with_formatted_error/1 :: (atom()) -> term()).	      
with_formatted_error(F) ->
    case catch F() of
	{'EXIT', Reason} ->
	    e_mod_inets:format_response(e_mod_gen:error_page(501, "", Reason));
	Response ->
	    e_mod_inets:format_response(Response)
    end.
