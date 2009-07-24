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

-define(STATUS_CODES, [
        {100, "Continue"},
        {101, "Switching Protocols"},
        {200, "OK"},
        {201, "Created"},
        {202, "Accepted"},
        {203, "Non-Authoritative Information"},
        {204, "No Content"},
        {205, "Reset Content"},
        {206, "Partial Content"},
        {300, "Multiple Choices"},
        {301, "Moved Permanently"},
        {302, "Found"},
        {303, "See Other"},
        {304, "Not Modified"},
        {305, "Use Proxy"},
        {307, "Temporary Redirect"},
        {400, "Bad Request"},
        {401, "Unauthorized"},
        {402, "Payment Required"},
        {403, "Forbidden"},
        {404, "Not Found"},
        {405, "Method Not Allowed"},
        {406, "Not Acceptable"},
        {407, "Proxy Authentication Required"},
        {408, "Request Time-out"},
        {409, "Conflict"},
        {410, "Gone"},
        {411, "Length Required"},
        {412, "Precondition Failed"},
        {413, "Request Entity Too Large"},
        {414, "Request-URI Too Large"},
        {415, "Unsupported Media Type"},
        {416, "Requested range not satisfiable"},
        {417, "Expectation Failed"},
        {500, "Internal Server Error"},
        {501, "Not Implemented"},
        {502, "Bad Gateway"},
        {503, "Service Unavailable"},
        {504, "Gateway Time-out"},
        {505, "HTTP Version not supported"}
    ]).
