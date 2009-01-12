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
%%% file    : e_fe_yaws.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%%-------------------------------------------------------------------
-module(e_fe_mod_yaws).

-export([out/1, arg_rewrite/1]).
-export([start/0]).

-include("yaws_api.hrl").
-include("yaws.hrl").
-include("eptic.hrl").

out(A) ->
    case e_mod_yaws:handle_args(#arg{headers = Headers} = A) of
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
            
	    e_dict:fset("__path", A#arg.appmoddata),
	    e_dict:fset("__cookie_key", ClientCookie),
	    e_dict:fset("__cacheable", is_cacheable()),

	    ControllerFun = fun() -> 
				    URL = [$/ | A#arg.appmoddata],
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
	    
	    e_mod_yaws:cleanup(),
	    [Result, CookieHeader];
	GetMore ->
	    GetMore
    end.

-spec(arg_rewrite/1 :: (tuple()) -> tuple()).	     
arg_rewrite(#arg{req = R} = Arg) ->
    {abs_path, URL} = R#http_request.path,
    check_docroot(Arg, URL).
    
-spec(start/0 :: () -> none()).	     
start() ->
    application:start(yaws),
    application:start(ssl),
    application:start(sasl),

    application:start(eptic),
    application:start(wpart),
    
    GC0 = yaws_config:make_default_gconf(false, "e_fe_server"),
    GC = GC0#gconf{logdir = "log"},
    SC1 = #sconf{port = 8080,
		 docroot = "docroot",
		 listen = {0, 0, 0, 0},
		 arg_rewrite_mod = ?MODULE,
		 appmods = [{"app", ?MODULE}]},
    SC2 = #sconf{port = 8081,
		 docroot = "docroot",
		 listen = {0,0,0,0},
		 arg_rewrite_mod = ?MODULE,
		 appmods = [{"app", ?MODULE}],
		 ssl = #ssl{keyfile = "priv/keys/host.key",
			    certfile = "priv/keys/host.cert",
			    password = ""}},
    yaws_api:setconf(GC, [[SC1], [SC2]]),
    
    application:start(eptic_fe).

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

-spec(rewrite_req/2 :: (tuple(), string()) -> tuple()).	     
rewrite_req(#arg{req = R} = Arg, Url) ->
    Arg#arg{req = R#http_request{path = {abs_path, "/app" ++ Url}}}.

-spec(is_cacheable/0 :: () -> bool()).	     
is_cacheable() ->
%% Here is the place where we can check, if the should skip cache
%% and redirect the request straight to the backend server.
%% 
%% We can decide wheter hit the cache or not after we inspect the
%% content of the session (e_fe_session:fget/1).
%%
%% As an example, all requests replies are cached.
    false.

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

