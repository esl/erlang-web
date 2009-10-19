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
%%% File    : e_mod_ewts.erl
%%% Author  : Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% Description : Browser's EWTS layer for Erlang Web. 
%%%
%%% Created : 19 Oct 2009 by michalptaszek <michalptaszek@coltrane.erlangsystems.com>
%%%-------------------------------------------------------------------
-module(e_mod_ewts).

-export([handle_request/1]).

-include_lib("ewts/include/request.hrl").
-include_lib("eptic/include/eptic.hrl").

-spec(handle_request/1 :: (tuple()) -> tuple()).
handle_request(Request) ->
    e_dict:init_state([{"get", Request#request.get_args},
		       {"post", Request#request.post_args}]),

    ClientCookie = cookie_up(Request#request.cookies),
    
    e_dict:fset("__https", Request#request.protocol == https),
    e_dict:fset("__cookie_key", ClientCookie),
    e_dict:fset("__ip", {0, 0, 0, 0}),
    e_dict:fset("__path", Request#request.url),
    e_dict:fset("__cookies", Request#request.cookies),
    
    ControllerFun = fun() ->
			    case e_mod_gen:handle_request(Request#request.url) of
				{ret_view, Ret, View} ->
				    controller_exec(Ret, View);
				[_Status, _Html] = Error ->
				    Error;
				{html, HTML} ->
				    controller_exec({content, html, HTML}, []);
				enoent ->
				    enoent;
				Else ->
				    controller_exec(Else, "")
			    end
		    end,
    
    Response = with_formatted_error(ControllerFun),
    
    CookieHeader = cookie_bind(ClientCookie),
    {ok, Dict} = e_dict:get_state(),
    cleanup(),
    
    Response#response{cookies = [CookieHeader],
		      req_dict = Dict}.

-spec(cookie_up/1 :: (list(tuple())) -> term()).
cookie_up(Cookies) ->
    case proplists:get_value(?COOKIE, Cookies) of
	undefiend ->
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

-spec(controller_exec/2 :: (e_mod_gen:controller_response(), string()) -> 
	     {list(tuple()), string()}).
controller_exec(Ret, View) ->
    case Ret of
	template ->
	    format_response(e_mod_gen:template(e_mod_gen:template_file(View), [],
					       e_conf:template_expander()));
	{redirect, URL} ->
	    #response{code = 302,
		      headers = [{"location", URL}, 
				 {"content_length", "0"}]};
	{content, html, Data} ->
	    Length = {"content_length", integer_to_list(erlang:iolist_size(Data))},
	    #response{headers = [{"content_type", "text/html"},
				 Length],
		      body = Data};
	{content, xml, Data} ->
	    Length = {"content_length", integer_to_list(erlang:iolist_size(Data))},
	    #response{headers = [{"content_type", "text/xml"},
				 Length],
		      body = Data};
	{content, text, Data} ->
	    Length = {"content_length", integer_to_list(erlang:iolist_size(Data))},
	    #response{headers = [{"content_type", "text/plain"},
				 Length],
		      body = Data};
	{content, pdf, Data} ->
	    Length = {"content_length", integer_to_list(erlang:iolist_size(Data))},
	    #response{headers = [{"content_type", "application/pdf"},
				 Length],
		      body = Data};
	{json, Data} ->
	    Content = e_json:encode(Data),
	    Length = {"content_length", integer_to_list(erlang:iolist_size(Content))},
	    #response{headers = [{"content_type", "application/json"},
				 Length],
		      body = Content};
	{template, Template} ->
	    format_response(e_mod_gen:template(Template, [],
					       e_conf:template_expander()));
	{custom, Custom} ->
	    Custom;
	{headers, Headers, NewRet} ->
	    Res = controller_exec(NewRet, View),
	    ReadyHeaders = create_headers(Headers, []),
	    #response{body = Res#response.body,
		      headers = ReadyHeaders ++ Res#response.headers};
	{error, Code} ->
	    format_response(e_mod_gen:error_page(Code, e_dict:fget("__path")))
    end.

-spec(format_response/1 :: (term()) -> term()).	     
format_response({html, HTML}) ->
    Length = {"content_length", integer_to_list(erlang:iolist_size(HTML))},
    #response{headers = [{"content_type", "text/html"},
			 Length],
	      body = HTML};
format_response([{status, Code}, {html, HTML}]) ->
    Length = {"content_length", integer_to_list(erlang:iolist_size(HTML))},
    #response{headers = [{"content_type", "text/html"},
			 Length],
	      body = HTML,
	      code = Code};
format_response(Else) ->
    Else.

-spec(with_formatted_error/1 :: (fun()) -> term()).	      
with_formatted_error(F) ->
    case catch F() of
	{'EXIT', Reason} ->
	    format_response(e_mod_gen:error_page(501, e_dict:fget("__path"), Reason));
	Response ->
	    format_response(Response)
    end.

-spec(create_headers/2 :: (list(tuple()), list(tuple())) -> list(tuple())).	     
create_headers([], Acc) ->
    Acc;
create_headers([{cookie, CookieName, CookieVal} | Rest], Acc) ->
    create_headers(Rest, [{"Set-cookie", CookieName ++ [$= | CookieVal ++ "; path=/"]} | Acc]);
create_headers([{cookie, CookieName, CookieVal, CookiePath} | Rest], Acc) ->
    create_headers(Rest, [{"Set-cookie", CookieName ++ [$= | CookieVal ++ "; path=" ++ CookiePath]} | Acc]);
create_headers([{cookie, CookieName, CookieVal, CookiePath, CookieExpDate} | Rest], Acc) ->
    create_headers(Rest, [{"Set-cookie", CookieName ++ [$= | CookieVal ++ "; path=" ++ CookiePath ++ "; expires=" ++ CookieExpDate]} | Acc]);
create_headers([{content_length, Len} | Rest], Acc) when is_integer(Len) ->
    create_headers(Rest, [{"Content-Length", integer_to_list(Len)} | Acc]);
create_headers([{content_length, Len} | Rest], Acc) when is_list(Len) ->
    create_headers(Rest, [{"Content-Length", Len} | Acc]);
create_headers([_ | Rest], Acc) ->
    create_headers(Rest, Acc).

-spec(cookie_bind/1 :: (string()) -> {string(), string()}).	     
cookie_bind(ClientCookie) ->
    e_mod_gen:bind_session(ClientCookie),
    {?COOKIE, ClientCookie}.

-spec(cleanup/0 :: () -> any()).
cleanup() ->
    e_dict:terminate_state().
