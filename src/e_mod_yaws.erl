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
%%% File	: e_mod_yaws.erl
%%% @author Martin Carlson <martin@erlang-consulting.com>
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Yaws server callback module.
%%% @see e_mod_gen
%%% @end
%%%-------------------------------------------------------------------
-module(e_mod_yaws).

-export([out/1]).
-export([arg_rewrite/1]).
-export([fe_request/2]).

-export([cookie_up/1, add_headers/2, cookie_bind/1, cleanup/0]).
-export([get_url/1]).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("yaws/include/yaws.hrl").
-include_lib("eptic/include/eptic.hrl").

%%====================================================================
%% API
%%====================================================================
%% @hidden
out(A) ->
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

            ClientCookie = cookie_up(Headers),
	    URL = get_url(A#arg.appmoddata),
            
	    e_dict:fset("__path", URL),
	    e_dict:fset("__cookie_key", ClientCookie),
	    e_dict:fset("__ip", element(1, A#arg.client_ip_port)),

	    ControllerFun = fun() -> 
				    case e_mod_gen:handle_request([$/ | URL]) of
					{ret_view, Ret, View} ->
					    controller_exec(Ret, View);
					[_Status, _Html] = Error ->
					    Error;
					{html, _HTML} = HTML ->
					    HTML;
					Else ->
					    controller_exec(Else, "")
				    end
			    end,
            Result = with_formatted_error(ControllerFun),
	    CookieHeader = cookie_bind(ClientCookie),

	    cleanup(),
	    [Result, CookieHeader];
	GetMore ->
	    GetMore
    end.

%% @hidden
-spec(fe_request/2 :: (tuple(), term()) -> {term(), term()}).	     
fe_request(#arg{headers = Headers} = A, Session) ->
    Cookie = get_eptic_cookie(Headers),
    e_session:update_session(Cookie, Session),
    
    Ret = out(A),
    
    {ok, NewSession} = e_session:get_session(Cookie),
    {Ret, NewSession}.

%% @hidden
-spec(arg_rewrite/1 :: (tuple()) -> tuple()).	     
arg_rewrite(#arg{req = R} = Arg) ->
    {abs_path, URL} = R#http_request.path,

    check_static(Arg, URL).

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

%%====================================================================
%% Internal functions
%%====================================================================
-spec(handle_args/1 :: (tuple()) -> {ok, list(tuple())} | tuple()).
handle_args(#arg{req = R} = Arg) ->
    case e_multipart_yaws:is_multipart(Arg) of
	true  -> 
	    case e_multipart_yaws:parse(Arg) of
		{ok, Data} ->
		    {ok, [
			  {"get", yaws_api:parse_query(Arg)},
			  {"post", Data}
			 ]};
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
    end.

-spec(controller_exec/2 :: (e_mod_gen:controller_response(), string()) -> list(tuple()) | 
									      {html, string()} |
									      {redirect, string()} |
									      {content, string(), string()} |
									      term()).
									      
controller_exec(Ret, View) ->
    case Ret of
	template ->
	    e_mod_gen:template(e_mod_gen:template_file(View), [], 
			       e_conf:template_expander());
	{redirect, URL} ->
	    {redirect, URL};
	{content, html, Data} ->
	    {content, "text/html", Data};
	{content, xml, Data} ->
	    {content, "application/xml", Data};
	{content, text, Data} ->
	    {content, "text/plain", Data};
	{content, pdf, Data} ->
	    {content, "application/pdf", Data};
	{json, Data} ->
	    {content, "text/plain", e_json:encode(Data)};
	{template, Template} ->
	    e_mod_gen:template(Template, [],
			       e_conf:template_expander());
	{custom, Custom} ->
	    Custom;
	{headers, Headers, NewRet} ->
	    [controller_exec(NewRet, View), add_headers(Headers, [])];
	{error, Code} ->
	    e_mod_gen:error_page(Code, e_dict:fget("__path"))
    end.

-spec(add_headers/2 :: (list(tuple()), list(tuple())) -> list(tuple())).	     
add_headers([], Acc) ->
    Acc;
add_headers([{cookie, CookieName, CookieVal} | Rest], Acc) ->
    add_headers(Rest, [set_user_cookie(CookieName, CookieVal) | Acc]);
add_headers([{cookie, CookieName, CookieVal, CookiePath} | Rest], Acc) ->
    add_headers(Rest, [set_user_cookie(CookieName, CookieVal, CookiePath) | Acc]);
add_headers([{cookie, CookieName, CookieVal, CookiePath, CookieExpDate} | Rest], Acc) ->
    add_headers(Rest, [set_user_cookie(CookieName, CookieVal, CookiePath, CookieExpDate) | Acc]);
add_headers([{content_length, Len} | Rest], Acc) ->
    add_headers(Rest, [set_content_length(Len) | Acc]);
add_headers([_ | Rest], Acc) ->
    add_headers(Rest, Acc).
	
%% @hidden
-spec(cookie_up/1 :: (tuple()) -> string()).	     
cookie_up(Headers) ->
    Cookies = split_cookies(Headers#headers.cookie),
    eptic:fset("__cookies", Cookies),

    case yaws_api:find_cookie_val(?COOKIE, Headers#headers.cookie) of
        [] -> 
	    ClientCookie = e_session:new_session([]),
            e_mod_gen:restore_session(ClientCookie),
            ClientCookie;
        ClientCookie ->
	    {ok, Session} = e_session:get_session(ClientCookie),
            if 
                Session == undefined -> 
                    NewCookie = e_session:new_session([]),
                    e_mod_gen:restore_session(NewCookie),
                    NewCookie;
                true ->
                    e_mod_gen:restore_session(ClientCookie),
                    ClientCookie
            end
    end.

-spec(get_eptic_cookie/1 :: (tuple()) -> string()).	     
get_eptic_cookie(Headers) ->
    yaws_api:find_cookie_val(?COOKIE, Headers#headers.cookie).

-spec(split_cookies/1 :: (string()) -> list(tuple())).	     
split_cookies([]) ->
    [];
split_cookies([Cookies0]) ->
    Cookies = string:tokens(Cookies0, ";"),
    lists:map(fun(Cookie) ->
		      list_to_tuple(string:tokens(string:strip(Cookie, both, $ ), "="))
	      end, Cookies).

-spec(with_formatted_error/1 :: (fun()) -> term()).	     
with_formatted_error(F) ->
    case catch F() of
	{'EXIT', Reason} ->
	    e_mod_gen:error_page(501, "", Reason);
	Result ->
	    Result
    end.

%% @hidden
-spec(cookie_bind/1 :: (string()) -> tuple()).	     
cookie_bind(ClientCookie) ->
    e_mod_gen:bind_session(ClientCookie),
    yaws_api:setcookie(?COOKIE,ClientCookie,"/").

-spec(set_user_cookie/2 :: (string(), string()) -> tuple()).	     
set_user_cookie(CookieName, CookieVal) ->
    yaws_api:setcookie(CookieName, CookieVal, "/").
-spec(set_user_cookie/3 :: (string(), string(), string()) -> tuple()).
set_user_cookie(CookieName, CookieVal, CookiePath) ->
    yaws_api:setcookie(CookieName, CookieVal, CookiePath).
-spec(set_user_cookie/4 :: (string(), string(), string(), string()) -> tuple()).	     
set_user_cookie(CookieName, CookieVal, CookiePath, CookieExpDate) ->
    yaws_api:setcookie(CookieName, CookieVal, CookiePath, CookieExpDate).

-spec(set_content_length/1 :: (integer() | string()) -> tuple()).	     
set_content_length(Length) ->
    {header, {content_length, Length}}.

-spec(cleanup/0 :: () -> true).	     
cleanup() ->
    e_multipart_yaws:terminate(),
    e_dict:terminate_state().

%% @hidden
-spec(get_url/1 :: (string()) -> string()).	     
get_url([$/ | Url]) -> Url;
get_url(Url)        -> Url.
