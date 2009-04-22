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
%%% file    : e_fe_mod_yaws.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%%-------------------------------------------------------------------
-module(e_fe_mod_yaws).

-export([out/1, arg_rewrite/1]).

-include("yaws_api.hrl").
-include("yaws.hrl").
-include("eptic.hrl").

out(A) ->
    e_logger:register_pid(self()),
    case handle_args(#arg{headers = Headers} = A) of
	{ok, Args} ->
	    e_dict:init_state(Args),

	    Clisock = A#arg.clisock,
	    if
		is_tuple(Clisock) andalso element(1, Clisock) == sslsocket ->
		    e_dict:fset("__https", true);
		true ->
		    e_dict:fset("__https", false)
	    end,

            ClientCookie = e_mod_yaws:cookie_up(Headers),
	    URL = e_mod_yaws:get_url(A#arg.appmoddata),

	    e_logger:log({?MODULE, {url, URL}}),
            
	    e_dict:fset("__path", URL),
	    e_dict:fset("__cookie_key", ClientCookie),
	    e_dict:fset("__cacheable", is_cacheable()),

	    ControllerFun = fun() -> 
				    URL = [$/ | URL],
				    case e_fe_mod_gen:handle_request(URL) of
					enoent ->
					    enoent;
					{ready, Ready} ->
					    Ready;
					{not_ready, NotReady, View} ->
					    controller_exec(NotReady, View, URL)
				    end
			    end,

	    Result = with_formatted_error(ControllerFun),
	    CookieHeader = e_mod_yaws:cookie_bind(ClientCookie),
	 
	    e_fe_proxy:cleanup_backend(e_multipart_yaws),
	    e_dict:terminate_state(),

	    e_logger:unregister_pid(self()),

	    [Result, CookieHeader];
	GetMore ->
	    e_logger:unregister_pid(self()),

	    GetMore
    end.

-spec(arg_rewrite/1 :: (tuple()) -> tuple()).	     
arg_rewrite(#arg{req = R} = Arg) ->
    {abs_path, URL} = R#http_request.path,
    check_docroot(Arg, URL).
    
-spec(check_docroot/2 :: (tuple(), string()) -> tuple()).	
check_docroot(Arg, Url) ->
%% Add here your docroot elements resolver.
%% If the URL matches some element from docroot, the Arg itself 
%% should be returned. 
%% Otherwise, the request will be handled by the e_fe_yaws.
    check_static(Arg, Url).
 %%   rewrite_req(Arg, Url).

-spec(check_static/2 :: (tuple(), string()) -> tuple()).	     
check_static(Arg = #arg{req = R}, [$/, $a, $p, $p | _] = URL) ->
    Arg#arg{req = R#http_request{path = {abs_path, "/app" ++ URL}}};
check_static(Arg = #arg{req = R}, URL) ->
    case e_dispatcher:is_static(URL) of
	true ->
	    Arg;
	false ->
	    Arg#arg{req = R#http_request{path = {abs_path, "/app" ++ URL}}}
    end.

-spec(is_cacheable/0 :: () -> bool()).	     
is_cacheable() ->
    case e_conf:get_conf(is_cacheable_mod) of
	undefined ->
	    true;
	Mod ->
	    Mod:is_cacheable()
    end.

-spec(handle_args/1 :: (tuple()) -> {ok, list(tuple())} | tuple()).
handle_args(#arg{req = R} = Arg) ->
    Res = case e_multipart_yaws:is_multipart(Arg) of
	      true  -> 
		  {ok, Node} = application:get_env(eptic_fe, be_server_name),
		  case rpc:call(Node, e_multipart_yaws, parse, [Arg]) of
		      {ok, Data} ->
			  e_cluster:synchronize_docroot(),
			  {ok, [
				{"get", yaws_api:parse_query(Arg)},
				{"post", Data}
			       ]};
		      {badrpc, Reason} ->
			  error_logger:error_msg("~p module, error during multipart parse, reason: ~p~n",
						 [?MODULE, Reason]),
			  {ok, [{"get", yaws_api:parse_query(Arg)}]};
		      GetMore ->
			  GetMore
		  end;
	      false when R#http_request.method == 'POST' ->
		  {ok, [
			{"get", yaws_api:parse_query(Arg)},
			{"post", yaws_api:parse_post(Arg)}
		       ]};
	      false ->
		  {ok, [{"get", yaws_api:parse_query(Arg)}]}
	  end,
    e_logger:log({?MODULE, {handle_args, Res}}),
    Res.

-spec(controller_exec/3 :: (term(), string(), string()) -> term()).
controller_exec({ret_view, Ret, View}, _, URL) ->
    Response = controller_exec(Ret, View),
    e_fe_cache:save_cache(URL, Response),
    Response;
controller_exec({html, _HTML} = HTML, _, URL) ->
    e_fe_cache:save_cache(URL, HTML),
    HTML;
controller_exec([_Status, _HTML] = Error, _, URL) ->
    e_fe_cache:save_cache(URL, Error),
    Error;
controller_exec(Else, View, URL) ->
    Response = controller_exec(Else, View),
    e_fe_cache:save_cache(URL, Response),
    Response.

-spec(controller_exec/2 :: (term(), string()) -> term()).	     
controller_exec(template, View) ->
    e_fe_mod_gen:view(View);
controller_exec({redirect, URL}, _) ->
    {redirect, URL};
controller_exec({content, html, Data}, _) ->
    {content, "text/html", Data};
controller_exec({content, text, Data}, _) ->
    {content, "text/plain", Data};
controller_exec({json, Data}, _) ->
    {content, "text/plain", e_json:encode(Data)};
controller_exec({template, Template}, _) ->
    e_fe_mod_gen:view(Template);
controller_exec({custom, Custom}, _) ->
    Custom;
controller_exec({headers, Headers, Ret}, View) ->
    [controller_exec(Ret, View), e_mod_yaws:add_headers(Headers, [])];
controller_exec({error, Code}, _) ->
    e_mod_gen:error_page(Code, e_dict:fget("__path")).

-spec(with_formatted_error/1 :: (atom()) -> term()).	     
with_formatted_error(F) ->
    case catch F() of
	{'EXIT', Reason} ->
	    e_mod_gen:error_page(501, "", Reason);
	Result ->
	    Result
    end.
