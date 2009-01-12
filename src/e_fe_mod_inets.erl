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

do(#mod{parsed_header = Headers, socket_type = Socket} = A) ->
    ok.
%%     Cookie = proplists:get_value("eptic_cookie", get_cookies(Headers), []),
%%     e_fe_session:load_session(Cookie).
    
  %%   if
%% 	is_tuple(Socket) andalso element(1, Socket) == ssl ->
%% 	    e_fe_proxy:request(A, e_mod_inets);
%% 	true ->
%% 	    case is_cacheable() of
%% 		false ->
%% 		    e_fe_cache:request(A, URL, e_mod_inets);
%% 		true ->
%% 		    e_fe_proxy:request(A, e_mod_inets)
%% 	    end
%%     end.

-spec(start/0 :: () -> none()).	     
start() ->
    application:start(ssl),
    application:start(sasl),

    application:set_env(inets, services, [{httpd, filename:join(code:priv_dir(eptic_fe), "inets.conf")},
					  {httpd, filename:join(code:priv_dir(eptic_fe), "inets_https.conf")}]),

    inets:start(),
    application:start(eptic_fe).

%% -spec(is_cacheable/0 :: () -> bool()).	     
%% is_cacheable() ->
%% %% Here is the place where we can check, if the should skip cache
%% %% and redirect the request straight to the backend server.
%% %% 
%% %% We can decide wheter hit the cache or not after we inspect the
%% %% content of the session (e_fe_session:fget/1).
%% %%
%% %% As an example, all requests replies are cached.
%%     false.


%% -spec(get_cookies/1 :: (list(tuple())) -> list(tuple())).			      
%% get_cookies(Headers) ->
%%     case lists:keysearch("cookie", 1, Headers) of
%% 	false ->
%% 	    [];
%% 	{_, {_, Cookies0}} ->
%% 	    Cookies = string:tokens(Cookies0, ";"),
%% 	    lists:map(fun(Cookie) ->
%% 			      list_to_tuple(string:tokens(string:strip(Cookie, both, $ ), "="))
%% 		      end, Cookies)
%%     end.
