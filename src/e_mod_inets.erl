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
%%% File	: e_mod_inets.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Inets server callback module.
%%% @end
%%%-------------------------------------------------------------------
-module(e_mod_inets).

-export([do/1]).
-export([start/0]).
-export([fe_request/2]).

-include_lib("inets/src/httpd.hrl").

-include("eptic.hrl").

%%====================================================================
%% API
%% @hidden
%%====================================================================
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

	    ClientCookie = cookie_up(Headers),

	    e_dict:fset("__path", URL),
	    e_dict:fset("__cookie_key", ClientCookie),

	    ControllerFun = fun() ->
				    case e_mod_gen:handle_request(A#mod.request_uri) of
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

	    case with_formatted_error(ControllerFun) of
		{NewHeaders, Result} ->
		    CookieHeader = cookie_bind(ClientCookie),
		    cleanup(),

		    {proceed, [{response, {response, [CookieHeader | NewHeaders], Result}}]};
		enoent ->
		    cookie_bind(ClientCookie),
		    cleanup(),

		    {proceed, A#mod.data}
	    end
    end.

%% @hidden
-spec(fe_request/2 :: (tuple(), term()) -> {term(), term()}).
fe_request(#mod{parsed_header = Headers} = A, Session) ->
    Cookie = proplists:get_value(?COOKIE, get_cookies(Headers), []),
    e_session:update_session(Cookie, Session),
    
    Ret = do(A),
    
    {ok, NewSession} = e_session:get_session(Cookie),
    {Ret, NewSession}.

%%
%% @spec start() -> none()
%% @doc The framework interactive mode starting function.
%% This function will start all required applications (including 
%% three main framework ones: eptic, wpart and wparts).
%% Moreover, it will set up Inets server to listen to the 
%% port specified in <i>project.conf</i> file.
%% The basic MIME types are set here as well.
%% @see e_conf:http_port/0
%%
-spec(start/0 :: () -> none()).  
start() ->
    application:start(ssl),
    application:start(sasl),

    application:start(eptic),
    application:start(wpart),
    application:start(wparts),
    application:start(crypto),

    application:set_env(inets, services, [{httpd, filename:join(code:priv_dir(eptic), "inets.conf")},
					  {httpd, filename:join(code:priv_dir(eptic), "inets_https.conf")}]),

%%   MimeTypes = [{"xml","text/xml"},
%% 		 {"txt","text/plain"},
%% 		 {"html","text/html"},
%% 		 {"htm","text/html"},
%% 		 {"css","text/css"},
%% 		 {"png","image/png"},
%% 		 {"jpeg","image/jpeg"},
%% 		 {"jpg","image/jpeg"},
%% 		 {"jpe","image/jpeg"},
%% 		 {"gif","image/gif"},
%% 		 {"bmp","image/bmp"},
%% 		 {"zip","application/zip"},
%% 		 {"tar","application/x-tar"},
%% 		 {"js","application/x-javascript"},
%% 		 {"pdf","application/pdf"},
%% 		 {"doc","application/msword"},
%% 		 {"swf","application/x-shockwave-flash"},
%% 		 {"wav","audio/x-wav"},
%% 		 {"mpeg", "video/mpeg"},
%% 		 {"mpg", "video/mpeg"},
%% 		 {"mpe", "video/mpeg"},
%% 		 {"ico", "image/x-icon"}],
%%     application:set_env(inets, services, [{httpd, 
%% 					   [{server_name, "erlang_web"},
%% 					    {server_root, e_conf:server_root()},
%% 					    {document_root, "docroot"},
%% 					    {bind_address, {0, 0, 0, 0}},
%% 					    {port, list_to_integer(e_conf:http_port())},
%% 					    {modules, [e_mod_inets,
%% 						       mod_get,
%% 						       mod_head,
%% 						       mod_log,
%% 						       mod_disk_log]},
%% 					    {mime_types, MimeTypes},
%% 					    {transfer_log,"log/access_log_http"},
%% 					    {error_log,"log/error_log_http"}]},
					  
%% 					  {httpd,
%% 					   [{server_name, "erlang_web_https"},
%% 					    {server_root, e_conf:server_root()},
%% 					    {document_root, "docroot"},
%% 					    {bind_address, {0, 0, 0, 0}},
%% 					    {port, list_to_integer(e_conf:https_port())},
%% 					    {modules, [e_mod_inets,
%% 						       mod_get,
%% 						       mod_head,
%% 						       mod_log,
%% 						       mod_disk_log]},
%% 					    {mime_types, MimeTypes},
%% 					    {transfer_log,"log/access_log_https"},
%% 					    {error_log,"log/error_log_https"},
%% 					    {ssl_certificate_file, "/home/michalp/dev/erlang_org/branches/erlangorg_2/priv/keys/host.cert"},
%% 					    {ssl_certificate_key_file, "/home/michalp/dev/erlang_org/branches/erlangorg_2/priv/keys/host.key"},
%% 					    {ssl_verify_client, 0},
%% 					    {socket_type, ssl}
%% 					   ]}]
%% 		       ),
    inets:start(),

    application:set_env(eptic, template_root, "templates").

%%====================================================================
%% Internal functions
%%====================================================================
-spec(handle_args/1 :: (tuple()) -> {ok, list(tuple())}).	     
handle_args(#mod{method = Method, entity_body = Post} = Mod) ->
    case Method of
	"POST" ->
	    {ok, [{"get", parse_get(Mod#mod.request_uri)},
		  {"post", parse_post(Post)}]};
	_ ->
	    {ok, [{"get", parse_get(Mod#mod.request_uri)}]}
    end.

-spec(controller_exec/2 :: (e_mod_gen:controller_response(), string()) -> {list(tuple()), string()}).	     
controller_exec(Ret, View) ->
    case Ret of
	template ->
	    format_response(e_mod_gen:template(e_mod_gen:template_file(View), [],
					       e_conf:template_expander()));
	{redirect, URL} ->
	    {[{code, 302}, {location, URL}, {content_length, "0"}], []};
	{content, html, Data} ->
	    Content = lists:flatten(Data),
	    Length = {content_length, integer_to_list(length(Content))},
	    {[{content_type, "text/html"}, {code, 200}, Length], Content};
	{content, text, Data} ->
	    Content = lists:flatten(Data),
	    Length = {content_length, integer_to_list(length(Content))},
	    {[{content_type, "text/plain"}, {code, 200}, Length], Content};
	{json, Data} ->
	    Content = e_json:encode(Data),
	    Length = {content_length, integer_to_list(length(Content))},
	    {[{content_type, "text/plain"}, {code, 200}, Length], Content};
	{template, Template} ->
	    format_response(e_mod_gen:template(Template, [],
					       e_conf:template_expander()));
	{custom, Custom} ->
	    Custom;
	{headers, Headers, NewRet} ->
	    {NewHeaders, ProperRet} = controller_exec(NewRet, View),
	    ReadyHeaders = create_headers(Headers, []),
	    {ReadyHeaders ++ NewHeaders, ProperRet};
	{error, Code} ->
	    e_mod_gen:error_page(Code, e_dict:fget("__path"))
    end.

-spec(format_response/1 :: (term()) -> term()).	     
format_response({html, HTML}) ->
    Content = lists:flatten(HTML),
    Length = {content_length, integer_to_list(length(Content))},
    {[{content_type, "text/html"}, {code, 200}, Length], Content};
format_response([{status, Code}, {html, HTML}]) ->
    Content = lists:flatten(HTML),
    Length = {content_length, integer_to_list(length(Content))},
    {[{content_type, "text/html"}, {code, Code}, Length], Content};
format_response(Else) ->
    Else.

-spec(with_formatted_error/1 :: (atom()) -> term()).	      
with_formatted_error(F) ->
    case catch F() of
	{'EXIT', Reason} ->
	    format_response(e_mod_gen:error_page(501, "", Reason));
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
create_headers([_ | Rest], Acc) ->
    create_headers(Rest, Acc).

-spec(parse_get/1 :: (string()) -> list(tuple())).	     
parse_get(URL) ->
    case string:chr(URL, $?) of
	0 ->
	    [];
	Pos ->
	    Get = string:tokens(string:sub_string(URL, Pos+1), [$&]),
	    lists:map(fun(Pair) ->
			      case string:tokens(Pair, [$=]) of
				  [Key, Val] ->
				      {Key, Val};
				  [Key | _] ->
				      {Key, ""}
			      end
		      end, Get)
    end.

-spec(parse_post/1 :: (string()) -> list(tuple())).	     
parse_post(String) ->
    case fetch_boundary(String) of
	{simple, Data} ->
	    httpd:parse_query(Data);
	{multipart, Boundary} ->
	    e_multipart_inets:get_multipart(String, Boundary)
    end.

-spec(fetch_boundary/1 :: (string()) -> {simple, string()} | {multipart, string()}).	     
fetch_boundary(Data) ->
    case string:str(Data, "\r\n") of
	0 -> 
	    {simple, Data};
	Pos -> 
	    {multipart, string:substr(Data, 1, Pos-1)}
    end.

-spec(cookie_up/1 :: (list(tuple())) -> term()).	     
cookie_up(Headers) ->
    Cookies = get_cookies(Headers),
    eptic:fset("__cookies", Cookies),

    case proplists:get_value(?COOKIE, Cookies) of
	false ->
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

-spec(cookie_bind/1 :: (string()) -> {string(), string()}).	     
cookie_bind(ClientCookie) ->
    e_mod_gen:bind_session(ClientCookie),
    {"Set-cookie", ?COOKIE ++ [$= | ClientCookie ++ "; path=/"]}.

-spec(cleanup/0 :: () -> none()).	     
cleanup() ->
    e_multipart_inets:terminate(),
    e_dict:terminate_state().

-spec(get_cookies/1 :: (list(tuple())) -> list(tuple())).			      
get_cookies(Headers) ->
    case lists:keysearch("cookie", 1, Headers) of
	false ->
	    [];
	{_, {_, Cookies0}} ->
	    Cookies = string:tokens(Cookies0, ";"),
	    lists:map(fun(Cookie) ->
			      list_to_tuple(string:tokens(string:strip(Cookie, both, $ ), "="))
		      end, Cookies)
    end.
