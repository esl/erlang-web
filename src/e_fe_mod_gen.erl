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

%%
%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%% @doc Generic module for all e_fe_mod_* servers.
%%
-module(e_fe_mod_gen).

-export([handle_request/1]).
-export([view/1]).

%%
%% @spec handle_request(Url :: string()) -> Response :: term()
%% @doc Handles the incoming request.
%% The docroot content and the static routes are always handled
%% by the frontend server. The dynamic ones are passed to the 
%% backend node. 
%%
-spec(handle_request/1 :: (string()) -> term()).	     
handle_request("/app/" ++ URL) ->
    case e_mod_gen:parse_url(URL) of
	{M, F, View} -> 
	    eptic:fset("__cache_type", normal),
	    dynamic_request(M, F, [], View, URL);
	{view, View} -> 
	    eptic:fset("__cache_type", persistent),
	    static_request(URL, View);
	{error, Error} -> 
	    eptic:fset("__cache_type", normal),
	    {ready, error_request(501, Error)}
    end;
handle_request(URL) ->
    {Type, Rule} = e_dispatcher:fe_dispatch(URL),
    eptic:fset("__cache_type", Type),
    
    case Rule of
	{error, Code, Path} -> {ready, error_request(Code, Path)};
	{M, F, A} -> dynamic_request(M, F, A, [], URL);
	{view, View} -> static_request(URL, View);
	invalid_url -> enoent  
    end.

-spec(static_request/2 :: (string(), string()) -> {ready, term()}).	     
static_request(URL, View) ->
    BURL = list_to_binary(URL),
    case e_fe_cache:request(BURL) of
	not_found ->
	    Response = e_fe_cache:ask_front_end(View),
	    e_fe_cache:save_cache(URL, Response),
	    {ready, Response};
	{cached, Cached} ->
	    {ready, Cached}
    end.

-spec(dynamic_request/5 :: (atom(), atom(), list(), string(), string()) -> {ready, term()} | {not_ready, term(), string()}).	     
dynamic_request(M, F, A, View, URL) ->
    BURL = list_to_binary(URL),
    case e_fe_cache:request(BURL) of
	not_found ->
	    Response = e_fe_cache:ask_back_end(M, F, A),
	    {not_ready, Response, View};
	{cached, Cached} ->
	    {ready, Cached}
    end.

-spec(error_request/2 :: (integer(), string()) -> term()).	     
error_request(404, Path) ->
    Error = e_mod_gen:error_page(404, Path),
    e_fe_cache:save_cache(persistent, Path, Error),
    Error;
error_request(Code, Path) ->
    e_mod_gen:error_page(Code, Path).

-spec(view/1 :: (string()) -> term()).	     
view(View) ->
    template(template_file(View)).

-spec(template_file/1 :: (string()) -> string()).	     
template_file(View) ->
    filename:join([
		   e_conf:template_root(),
		   e_mod_gen:sanitize_file_name(View)
		  ]).

-spec(template/1 :: (string()) -> term()).
template(File) ->
    case e_cache:read_file(File) of
	{error, Error} ->
	    e_mod_gen:error_page(404, File, {e_cache_error, Error});
	E ->
	    {html, wpart_xs:process_xml(E)}
    end.
