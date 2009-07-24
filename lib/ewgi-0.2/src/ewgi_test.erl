%%%-------------------------------------------------------------------
%%% File    : ewgi_application.erl
%%% Authors : Hunter Morris <huntermorris@gmail.com>
%%% License :
%%% The contents of this file are subject to the Mozilla Public
%%% License Version 1.1 (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of
%%% the License at http://www.mozilla.org/MPL/
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and
%%% limitations under the License.
%%% The Initial Developer of the Original Code is S.G. Consulting
%%% srl. Portions created by S.G. Consulting s.r.l. are Copyright (C)
%%% 2007 S.G. Consulting srl. All Rights Reserved.
%%%
%%% @doc 
%%% <p>ewgi test applications</p>
%%%
%%% @end
%%%
%%% Created : 05 July 2009 by Hunter Morris <huntermorris@gmail.com>
%%%-------------------------------------------------------------------
-module(ewgi_test).

%% Test EWGI applications
-export([testapp/1, testapp_chunked/1]).

-include_lib("ewgi.hrl").

testapp(C) ->
    Body = io_lib:format("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\"><html><body><h1>EWGI Context</h1><h2>Request</h2>~s</body></html>", [htmlise(C)]),
    Stat = {200, "OK"},
    H = [{"content-type", "text/html"}],
    ewgi_api:response_message_body(
      Body, ewgi_api:response_headers(
              H, ewgi_api:response_status(Stat, C))).

testapp_chunked(C0) ->
    C = testapp(C0),
    B = ewgi_api:response_message_body(C),
    ewgi_api:response_message_body(list_to_stream(B), C).

htmlise(C) ->
    iolist_to_binary(
      ["<dl class=\"request\">",
       io_lib:format("<dt>url_scheme</dt><dd><pre>~s</pre></dd>", [ewgi_api:url_scheme(C)]),
       io_lib:format("<dt>request_method</dt><dd><pre>~s</pre></dd>", [ewgi_api:request_method(C)]),
       io_lib:format("<dt>path_info</dt><dd><pre>~s</pre></dd>", [ewgi_api:path_info(C)]),
       io_lib:format("<dt>query_string</dt><dd><pre>~s</pre></dd>", [ewgi_api:query_string(C)]),
       io_lib:format("<dt>remote_addr</dt><dd><pre>~s</pre></dd>", [ewgi_api:remote_addr(C)]),
       io_lib:format("<dt>auth_type</dt><dd><pre>~s</pre></dd>", [ewgi_api:auth_type(C)]),
       io_lib:format("<dt>content_length</dt><dd><pre>~p</pre></dd>", [ewgi_api:content_length(C)]),
       io_lib:format("<dt>content_type</dt><dd><pre>~s</pre></dd>", [ewgi_api:content_type(C)]),
       io_lib:format("<dt>gateway_interface</dt><dd><pre>~s</pre></dd>", [ewgi_api:gateway_interface(C)]),
       io_lib:format("<dt>path_translated</dt><dd><pre>~s</pre></dd>", [ewgi_api:path_translated(C)]),
       io_lib:format("<dt>remote_host</dt><dd><pre>~s</pre></dd>", [ewgi_api:remote_host(C)]),
       io_lib:format("<dt>remote_ident</dt><dd><pre>~s</pre></dd>", [ewgi_api:remote_ident(C)]),
       io_lib:format("<dt>remote_user</dt><dd><pre>~s</pre></dd>", [ewgi_api:remote_user(C)]),
       io_lib:format("<dt>remote_user_data</dt><dd><pre>~s</pre></dd>", [ewgi_api:remote_user_data(C)]),
       io_lib:format("<dt>script_name</dt><dd><pre>~s</pre></dd>", [ewgi_api:script_name(C)]),
       io_lib:format("<dt>server_name</dt><dd><pre>~s</pre></dd>", [ewgi_api:server_name(C)]),
       io_lib:format("<dt>server_port</dt><dd><pre>~s</pre></dd>", [ewgi_api:server_port(C)]),
       io_lib:format("<dt>other http headers</dt><dd>~s</dd>", [htmlise_data("http_headers", ewgi_api:get_all_headers(C))]),
       io_lib:format("<dt>server_protocol</dt><dd><pre>~s</pre></dd>", [ewgi_api:server_protocol(C)]),
       io_lib:format("<dt>server_software</dt><dd><pre>~s</pre></dd>", [ewgi_api:server_software(C)]),
       io_lib:format("<dt>ewgi version</dt><dd><pre>~p</pre></dd>", [ewgi_api:version(C)]),
       io_lib:format("<dt>ewgi extra data</dt><dd>~s</dd>", [htmlise_data("request_data", ewgi_api:get_all_data(C))]),
       "</dl>"]).

htmlise_data(Name, L) when is_list(L) ->
    ["<dl class=\"", Name, "\">",
     [io_lib:format("<dt>~s</dt><dd><pre>~p</pre><dd>", [K, V]) || {K, V} <- L],
     "</dl>"];
htmlise_data(Name, T) ->
    case gb_trees:to_list(T) of
        [] -> [];
        L -> htmlise_data(Name, L)
    end.

list_to_stream(L) when is_list(L) ->
    fun() ->
            case L of
                [H|T] ->
                    {H, list_to_stream(T)};
                [] ->
                    {}
            end
    end.
