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
%%% File    : e_mod_ewgi.erl
%%% @author <info@erlang-consulting.com>
%%% @end
%%%-------------------------------------------------------------------

-record(ewgi_spec, {
        read_input,
        write_error,
        url_scheme,
        version,
        data % set
    }).

-record(ewgi_http_headers, {
        http_accept,
        http_cookie,
        http_host,
        http_if_modified_since,
        http_user_agent,
        http_x_http_method_override,
        other % multiset
    }).

-record(ewgi_request, {
        auth_type,
        content_length,
        content_type,
        ewgi=#ewgi_spec{},
        gateway_interface,
        http_headers=#ewgi_http_headers{},
        path_info,
        path_translated,
        query_string,
        remote_addr,
        remote_host,
        remote_ident,
        remote_user,
        remote_user_data,
        request_method,
        script_name,
        server_name,
        server_port,
        server_protocol,
        server_software
    }).

-record(ewgi_response, {
        status={200, "OK"},
        headers=[],
        message_body,
        err
    }).

-record(ewgi_context, {
        request=#ewgi_request{},
        response=#ewgi_response{}
    }).
