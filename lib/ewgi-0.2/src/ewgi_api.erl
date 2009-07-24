%%%-------------------------------------------------------------------
%%% File    : ewgi_api.erl
%%% Authors : Filippo Pacini <filippo.pacini@gmail.com>
%%%           Hunter Morris <huntermorris@gmail.com>
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
%%% <p>ewgi API. Defines a low level CGI like API.</p>
%%%
%%% @end
%%%
%%% Created : 10 Oct 2007 by Filippo Pacini <filippo.pacini@gmail.com>
%%%-------------------------------------------------------------------
-module(ewgi_api).

-include_lib("ewgi.hrl").

%% Record helpers
-export([empty_request/0, empty_response/0, context/2, request/2, response/2,
         request/1, response/1]).

-export([response_headers/1, response_message_body/1, response_status/1,
         response_error/1]).

-export([response_headers/2, response_message_body/2, response_status/2,
         response_error/2]).

%% Request 'get' methods
-export([auth_type/1, content_length/1, content_type/1, gateway_interface/1,
         path_info/1, path_translated/1, query_string/1, remote_addr/1,
         remote_host/1, remote_ident/1, remote_user/1, remote_user_data/1,
         request_method/1, script_name/1, server_name/1, server_port/1,
         server_protocol/1, server_software/1]).

%% Request 'set' methods
-export([auth_type/2, content_length/2, content_type/2, gateway_interface/2,
         path_info/2, path_translated/2, query_string/2, remote_addr/2,
         remote_host/2, remote_ident/2, remote_user/2, remote_user_data/2,
         request_method/2, script_name/2, server_name/2, server_port/2,
         server_protocol/2, server_software/2]).

%% Additional request methods
-export([get_header_value/2, set_header/3, insert_header/3,
         get_all_headers/1, read_input/1, read_input/3, read_input_string/2,
         write_error/2, url_scheme/1, version/1, get_all_data/1, find_data/2,
         find_data/3, store_data/3]).

%% Server methods
-export([server_request_foldl/4]).

%% Utility methods
-export([parse_qs/1, parse_post/1, urlencode/1, quote/1, normalize_header/1,
         unquote_path/1, path_components/3, urlsplit/1]).

%%====================================================================
%% API
%%====================================================================
-spec empty_request() -> ewgi_request().
empty_request() ->
    {'ewgi_request', undefined, undefined, undefined,
     empty_ewgi_spec(), undefined, empty_http_headers(), undefined,
     undefined, undefined, undefined, undefined, undefined, undefined,
     undefined, undefined, undefined, undefined, undefined, undefined,
     undefined}.

-spec empty_response() -> ewgi_response().
empty_response() ->
    {'ewgi_response', undefined, [], undefined, undefined}.

-spec context(ewgi_request(), ewgi_response()) -> ewgi_context().
context(Request, Response) when ?IS_EWGI_REQUEST(Request),
                                ?IS_EWGI_RESPONSE(Response) ->
    {'ewgi_context', Request, Response}.

-spec request(ewgi_request(), ewgi_context()) -> ewgi_context().
request(Req, Ctx) when ?IS_EWGI_REQUEST(Req), ?IS_EWGI_CONTEXT(Ctx) ->
    ?SET_EWGI_REQUEST(Req, Ctx).

-spec response(ewgi_response(), ewgi_context()) -> ewgi_context().
response(Rsp, Ctx) when ?IS_EWGI_RESPONSE(Rsp),
                        ?IS_EWGI_CONTEXT(Ctx) ->
    ?SET_EWGI_RESPONSE(Rsp, Ctx).

-spec response(ewgi_context()) -> ewgi_response().
response(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_EWGI_RESPONSE(Ctx).

-spec request(ewgi_context()) -> ewgi_request().
request(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_EWGI_REQUEST(Ctx).

-spec response_headers(ewgi_context()) -> ewgi_header_list().
response_headers(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_RESPONSE_HEADERS(response(Ctx)).

-spec response_status(ewgi_context()) -> ewgi_status().
response_status(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_RESPONSE_STATUS(response(Ctx)).

-spec response_message_body(ewgi_context()) -> ewgi_message_body().
response_message_body(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_RESPONSE_MESSAGE_BODY(response(Ctx)).

-spec response_error(ewgi_context()) -> any().
response_error(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_RESPONSE_ERROR(response(Ctx)).

-spec response_headers(ewgi_header_list(), ewgi_context()) -> ewgi_context().
response_headers(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    response(?SET_RESPONSE_HEADERS(V, response(Ctx)), Ctx).

-spec response_status(ewgi_status(), ewgi_context()) -> ewgi_context().
response_status(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    response(?SET_RESPONSE_STATUS(V, response(Ctx)), Ctx).

-spec response_message_body(ewgi_message_body(), ewgi_context()) -> ewgi_context().
response_message_body(V,  Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    response(?SET_RESPONSE_MESSAGE_BODY(V, response(Ctx)), Ctx).

-spec response_error(any(), ewgi_context()) -> ewgi_context().
response_error(V,  Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    response(?SET_RESPONSE_ERROR(V, response(Ctx)), Ctx).

-spec auth_type(ewgi_context()) -> ewgi_val().
auth_type(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_AUTH_TYPE(request(Ctx)).

-spec content_length(ewgi_context()) -> non_neg_integer().
content_length(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_CONTENT_LENGTH(request(Ctx)).

-spec content_type(ewgi_context()) -> ewgi_val().
content_type(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_CONTENT_TYPE(request(Ctx)).

-spec gateway_interface(ewgi_context()) -> ewgi_val().
gateway_interface(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_GATEWAY_INTERFACE(request(Ctx)).

-spec path_info(ewgi_context()) -> ewgi_val().
path_info(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_PATH_INFO(request(Ctx)).

-spec path_translated(ewgi_context()) -> ewgi_val().
path_translated(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_PATH_TRANSLATED(request(Ctx)).

-spec query_string(ewgi_context()) -> ewgi_val().
query_string(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_QUERY_STRING(request(Ctx)).

-spec remote_addr(ewgi_context()) -> ewgi_val().
remote_addr(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_REMOTE_ADDR(request(Ctx)).

-spec remote_host(ewgi_context()) -> ewgi_val().
remote_host(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_REMOTE_HOST(request(Ctx)).

-spec remote_ident(ewgi_context()) -> ewgi_val().
remote_ident(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_REMOTE_IDENT(request(Ctx)).

-spec remote_user(ewgi_context()) -> ewgi_val().
remote_user(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_REMOTE_USER(request(Ctx)).

-spec remote_user_data(ewgi_context()) -> ewgi_val().
remote_user_data(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_REMOTE_USER_DATA(request(Ctx)).

-spec request_method(ewgi_context()) -> ewgi_request_method().
request_method(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_REQUEST_METHOD(request(Ctx)).

-spec script_name(ewgi_context()) -> ewgi_val().
script_name(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_SCRIPT_NAME(request(Ctx)).

-spec server_name(ewgi_context()) -> ewgi_val().
server_name(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_SERVER_NAME(request(Ctx)).

-spec server_port(ewgi_context()) -> ewgi_val().
server_port(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_SERVER_PORT(request(Ctx)).

-spec server_protocol(ewgi_context()) -> ewgi_val().
server_protocol(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_SERVER_PROTOCOL(request(Ctx)).

-spec server_software(ewgi_context()) -> ewgi_val().
server_software(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_SERVER_SOFTWARE(request(Ctx)).

-spec headers(ewgi_context()) -> ewgi_http_headers().
headers(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_HTTP_HEADERS(request(Ctx)).

-spec headers(ewgi_http_headers(), ewgi_context()) -> ewgi_context().
headers(H, Ctx) when ?IS_HTTP_HEADERS(H), ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_HTTP_HEADERS(H, request(Ctx)), Ctx).

-spec get_header_value(string(), ewgi_context()) -> ewgi_header_val().
get_header_value(Hdr0, Ctx) when is_list(Hdr0), ?IS_EWGI_CONTEXT(Ctx) ->
    Hdr = string:to_lower(Hdr0),
    get_header1(Hdr, Ctx).

get_header1("accept", Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_HTTP_ACCEPT(headers(Ctx));
get_header1("cookie",  Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_HTTP_COOKIE(headers(Ctx));
get_header1("host", Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_HTTP_HOST(headers(Ctx));
get_header1("if-modified-since", Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_HTTP_IF_MODIFIED_SINCE(headers(Ctx));
get_header1("user-agent", Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_HTTP_USER_AGENT(headers(Ctx));
get_header1("x-http-method-override", Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_HTTP_X_HTTP_METHOD_OVERRIDE(headers(Ctx));
get_header1(Hdr, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    case gb_trees:lookup(Hdr, ?GET_HTTP_OTHER(headers(Ctx))) of
        {value, V} when is_list(V) ->
            {_, V1} = lists:unzip(V),
            string:join(V1, ", ");
        none ->
            undefined
    end.

insert_header(K0, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    K = string:to_lower(K0),
    insert_header1(K, K0, V, Ctx).

combine_headers(K, V, Ctx) ->
    case get_header1(K, Ctx) of
        undefined ->
                 V;
        S when is_list(S) ->
            string:join([S, V], ", ")
    end.

insert_header1("accept"=K, _, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    V1 = combine_headers(K, V, Ctx),
    headers(?SET_HTTP_ACCEPT(V1, headers(Ctx)), Ctx);
insert_header1("cookie"=K,  _, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    V1 = combine_headers(K, V, Ctx),
    headers(?SET_HTTP_COOKIE(V1, headers(Ctx)), Ctx);
insert_header1("host"=K, _, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    V1 = combine_headers(K, V, Ctx),
    headers(?SET_HTTP_HOST(V1, headers(Ctx)), Ctx);
insert_header1("if-modified-since"=K, _, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    V1 = combine_headers(K, V, Ctx),
    headers(?SET_HTTP_IF_MODIFIED_SINCE(V1, headers(Ctx)), Ctx);
insert_header1("user-agent"=K, _, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    V1 = combine_headers(K, V, Ctx),
    headers(?SET_HTTP_USER_AGENT(V1, headers(Ctx)), Ctx);
insert_header1("x-http-method-override"=K, _, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    V1 = combine_headers(K, V, Ctx),
    headers(?SET_HTTP_X_HTTP_METHOD_OVERRIDE(V1, headers(Ctx)), Ctx);
insert_header1(K, K0, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    D = t_insert_header(K, {K0, V}, ?GET_HTTP_OTHER(headers(Ctx))),
    headers(?SET_HTTP_OTHER(D, headers(Ctx)), Ctx).

set_header(K0, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    K = string:to_lower(K0),
    set_header1(K, K0, V, Ctx).

set_header1("accept", _, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    headers(?SET_HTTP_ACCEPT(V, headers(Ctx)), Ctx);
set_header1("cookie",  _, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    headers(?SET_HTTP_COOKIE(V, headers(Ctx)), Ctx);
set_header1("host", _, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    headers(?SET_HTTP_HOST(V, headers(Ctx)), Ctx);
set_header1("if-modified-since", _, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    headers(?SET_HTTP_IF_MODIFIED_SINCE(V, headers(Ctx)), Ctx);
set_header1("user-agent", _, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    headers(?SET_HTTP_USER_AGENT(V, headers(Ctx)), Ctx);
set_header1("x-http-method-override", _, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    headers(?SET_HTTP_X_HTTP_METHOD_OVERRIDE(V, headers(Ctx)), Ctx);
set_header1(K, K0, V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    D = t_enter_header(K, {K0, V}, ?GET_HTTP_OTHER(headers(Ctx))),
    headers(?SET_HTTP_OTHER(D, headers(Ctx)), Ctx).

auth_type(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_AUTH_TYPE(V, request(Ctx)), Ctx).

content_length(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_CONTENT_LENGTH(V, request(Ctx)), Ctx).

content_type(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_CONTENT_TYPE(V, request(Ctx)), Ctx).

gateway_interface(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_GATEWAY_INTERFACE(V, request(Ctx)), Ctx).

path_info(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_PATH_INFO(V, request(Ctx)), Ctx).

path_translated(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_PATH_TRANSLATED(V, request(Ctx)), Ctx).

query_string(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_QUERY_STRING(V, request(Ctx)), Ctx).

remote_addr(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_REMOTE_ADDR(V, request(Ctx)), Ctx).

remote_host(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_REMOTE_HOST(V, request(Ctx)), Ctx).

remote_ident(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_REMOTE_IDENT(V, request(Ctx)), Ctx).

remote_user(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_REMOTE_USER(V, request(Ctx)), Ctx).

remote_user_data(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_REMOTE_USER_DATA(V, request(Ctx)), Ctx).

request_method(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_REQUEST_METHOD(V, request(Ctx)), Ctx).

script_name(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_SCRIPT_NAME(V, request(Ctx)), Ctx).

server_name(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_SERVER_NAME(V, request(Ctx)), Ctx).

server_port(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_SERVER_PORT(V, request(Ctx)), Ctx).

server_protocol(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_SERVER_PROTOCOL(V, request(Ctx)), Ctx).

server_software(V, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_SERVER_SOFTWARE(V, request(Ctx)), Ctx).

get_all_headers(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    H = headers(Ctx),
    Other = gb_trees:to_list(?GET_HTTP_OTHER(H)),
    Acc = [{K, string:join(V, ", ")} || {K, {_, V}} <- [{K0, lists:unzip(V0)} || {K0, V0} <- Other, is_list(V0)]],
    L = [{"accept", get_header_value("accept", Ctx)},
         {"cookie", get_header_value("cookie", Ctx)},
         {"host", get_header_value("host", Ctx)},
         {"if-modified-since", get_header_value("if-modified-since", Ctx)},
         {"user-agent", get_header_value("user-agent", Ctx)},
         {"x-http-method-override", get_header_value("x-http-method-override", Ctx)}|Acc],
    lists:filter(fun({_, undefined}) -> false; (_) -> true end, L).

-spec ewgi_spec(ewgi_context()) -> ewgi_spec().
ewgi_spec(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_EWGI(request(Ctx)).

-spec ewgi_spec(ewgi_spec(), ewgi_context()) -> ewgi_context().
ewgi_spec(E, Ctx) when ?IS_EWGI_SPEC(E), ?IS_EWGI_CONTEXT(Ctx) ->
    request(?SET_EWGI(E, request(Ctx)), Ctx).

-spec read_input(ewgi_context()) -> ewgi_ri_callback() | 'undefined'.
read_input(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_EWGI_READ_INPUT(ewgi_spec(Ctx)).

-spec read_input(ewgi_ri_callback(), non_neg_integer(), ewgi_context()) -> ewgi_ri_callback() | 'undefined'.
read_input(Callback, Length, Ctx) when ?IS_EWGI_CONTEXT(Ctx),
                                       is_function(Callback, 1),
                                       is_integer(Length),
                                       Length >= 0 ->
    case read_input(Ctx) of
        F when is_function(F, 2) ->
            F(Callback, Length);
        undefined ->
            undefined
    end.

%% @spec read_input_string(non_neg_integer(), ewgi_context()) -> string() | {error, no_input}
%% @doc Reads the client message body into a string from the EWGI context.
-spec read_input_string(non_neg_integer(), ewgi_context()) -> [byte()] | {'error', 'no_input'}.
read_input_string(L, Ctx) when is_integer(L), L >= 0, ?IS_EWGI_CONTEXT(Ctx) ->
    case read_input(read_input_string_cb([]), L, Ctx) of
        undefined ->
            {error, no_input};
        Iol ->
            Bin = iolist_to_binary(Iol),
            binary_to_list(Bin)
    end.

-spec read_input_string_cb(list()) -> ewgi_ri_callback().
read_input_string_cb(Acc) ->
    F = fun(eof) ->
                lists:reverse(Acc);
           ({data, B}) ->
                read_input_string_cb([B|Acc])
        end,
    F.

write_error(Msg, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    F = ?GET_EWGI_WRITE_ERROR(ewgi_spec(Ctx)),
    F(Msg).

url_scheme(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_EWGI_URL_SCHEME(ewgi_spec(Ctx)).

version(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_EWGI_VERSION(ewgi_spec(Ctx)).

get_all_data(Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    ?GET_EWGI_DATA(ewgi_spec(Ctx)).

find_data(Key, Ctx) ->
    find_data(Key, Ctx, undefined).

find_data(Key, Ctx, Default) when ?IS_EWGI_CONTEXT(Ctx) ->
    case gb_trees:lookup(Key, get_all_data(Ctx)) of
        {value, V} ->
            V;
        none ->
            Default
    end.

store_data(Key, Val, Ctx) when ?IS_EWGI_CONTEXT(Ctx) ->
    D = gb_trees:enter(Key, Val, get_all_data(Ctx)),
    ewgi_spec(?SET_EWGI_DATA(D, ewgi_spec(Ctx)), Ctx).

%%--------------------------------------------------------------------
%% @spec parse_qs(string()|binary()) -> [proplist()]
%%
%% @doc Parse a query string. Calls parse_data to do the job.
%% @end
%%--------------------------------------------------------------------
parse_qs(ToParse) ->
    parse_data(ToParse).

%%--------------------------------------------------------------------
%% @spec parse_post(string()|binary()) -> [proplist()]
%%
%% @doc Parse application/x-www-form-urlencoded data. 
%% Calls parse_data to do the job.
%% @end
%%--------------------------------------------------------------------
parse_post(ToParse) ->
    parse_data(ToParse).

%%--------------------------------------------------------------------
%% @spec parse_data(string()|binary()) -> [proplist()]
%%
%% @doc Parse a query string or application/x-www-form-urlencoded data.
%% @end
%%--------------------------------------------------------------------
parse_data(undefined) ->
    [];
parse_data(Binary) when is_binary(Binary) ->
    parse_data(binary_to_list(Binary), []);
parse_data(String) ->
    parse_data(String, []).

parse_data([], Acc) ->
    lists:reverse(Acc);
parse_data(String, Acc) ->
    {{Key, Val}, Rest} = parse_kv(String),
    parse_data(Rest, [{Key, Val} | Acc]).


%%--------------------------------------------------------------------
%% @spec urlencode(proplist()) -> string()
%%
%% @doc URL encodes a proplist of parameters.
%% @end
%%--------------------------------------------------------------------
-spec urlencode(ewgi_proplist()) -> string().
urlencode(Props) ->
    QuotedL = [[quote(K), $=, quote(V)] || {K, V} <- Props],
    lists:flatten(join(QuotedL, $&)).

%%--------------------------------------------------------------------
%% @spec quote(term()) -> string()
%%
%% @doc URL encodes the given term.
%% @end
%%--------------------------------------------------------------------
-spec quote(ewgi_propval()) -> string().
quote(Term) when is_atom(Term) ->
    quote(atom_to_list(Term));
quote(Term) when is_integer(Term) ->
    quote(integer_to_list(Term));
quote(Term) when is_binary(Term) ->
    quote(binary_to_list(Term));
quote(Term) when is_list(Term) ->
    quote(Term, []).

-spec quote(string(), string()) -> string().
quote([], Acc) ->
    lists:reverse(Acc);
%% low alpha chars
quote([H|Rest], Acc) when H >= $a, H =< $z ->
    quote(Rest, [H|Acc]);
%% hialpha chars 
quote([H|Rest], Acc) when H >= $A, H =< $Z ->
    quote(Rest, [H|Acc]);
%% digit chars
quote([H|Rest], Acc) when H >= $0, H =< $9 ->
    quote(Rest, [H|Acc]);
%% safe chars
quote([H|Rest], Acc) when H =:= $-; H=:=$.; H=:=$_; H=:=$~ ->
    quote(Rest, [H|Acc]);
%% space
quote([$\s|Rest], Acc) ->
    quote(Rest, [$+ | Acc]);
%% other characters (convert to hex)
quote([H|Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<H>>,
    quote(Rest, [to_hex(Lo), to_hex(Hi), $\% | Acc]).


%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec parse_kv(String::string()) -> parsed()|{error, Reason}
%%
%% @type parsed() = {ok, proplist(), Rest::string()}
%%
%% @doc Parser for kv pairs found in query strings and body data.
%% returns the first proplist parsed from String or an error. 
%% @end
%%--------------------------------------------------------------------
-spec parse_kv(string()) -> {{string(), string()}, string()}.
parse_kv(String) ->
    P = and_parser([until(fun is_equal/1), until(fun is_amp/1)]),
    {ok, [K, V], Rest} = P(String),
    {{unquote(K), unquote(V)}, Rest}.

%%--------------------------------------------------------------------
%% @spec and_parser(Rules::rules()) -> parsed()|{error, Reason}
%%
%% @type rules() = [rule()]
%%       rule()  = function(template()).
%%
%% @doc and_parser of Rules. 
%% Applies each Rule in sequence to the Template passed. 
%% If a rule fails returns an error.
%% @end
%%--------------------------------------------------------------------
-type parser() :: fun((list()) -> {'ok', list(), list()}).
-spec and_parser([parser()]) -> parser().
and_parser(Rules) ->
    fun(Tmpl) ->
	    and_parser(Rules, Tmpl, [])
    end.

-spec and_parser(list(), list(), list()) -> {'ok', list(), list()}.
and_parser([], Tmpl, SoFar) ->
    {ok, lists:reverse(SoFar), Tmpl};
and_parser([Rule|T], Tmpl, SoFar) ->
    {ok, Tok, Rest} = Rule(Tmpl),
    and_parser(T, Rest, [Tok|SoFar]).

%%--------------------------------------------------------------------
%% @spec until(predicate()) -> parsed()|{error, Reason}
%%
%% @type predicate() = function(template()).
%%
%% @doc until predicate P: 
%% output what it gets until P(H) is true.
%% @end
%%--------------------------------------------------------------------
-type predicate() :: fun((list()) -> {'true', list()} | 'false').
-spec until(predicate()) -> parser().
until(P) ->
    fun (Tmpl) -> until(P, Tmpl, []) end.

-spec until(predicate(), list(), list()) -> {'ok', list(), list()}.
until(_P, [], Parsed) -> %% end of string so end parsing
    {ok, lists:reverse(Parsed), []};
until(P, String, Parsed) ->
    case P(String) of
	{true, Rest} ->
	    {ok, lists:reverse(Parsed), Rest};
	_ ->
            [H|Rest] = String,
	    until(P, Rest, [H|Parsed])
    end.

%%--------------------------------------------------------------------
%% @spec is_equal(string()) -> bool()
%%
%% @doc Match = character at the head of string.
%% @end
%%--------------------------------------------------------------------
-spec is_equal(string()) -> bool().
is_equal([$=|Rest]) ->
    {true, Rest};
is_equal(_) ->
    false.

%%--------------------------------------------------------------------
%% @spec is_amp(string()) -> bool()
%%
%% @doc Match &amp; character or &amp;amp; entity at the beginning of string.
%% @end
%%--------------------------------------------------------------------
-spec is_amp(string()) -> bool().
is_amp("&amp;"++Rest) ->
    {true, Rest};
is_amp([$&|Rest]) ->
    {true, Rest};
is_amp(_) ->
    false.


%%--------------------------------------------------------------------
%% @spec unquote(string()) -> string()
%%
%% @doc URL decodes the given term. 
%% Used to parse query strings and application/x-www-form-urlencoded data. 
%% @end
%%--------------------------------------------------------------------
unquote(Val) when is_binary(Val) ->
    unquote(binary_to_list(Val), []);
unquote(Val) ->
    unquote(Val, []).

unquote([], Acc) ->
    lists:reverse(Acc);
unquote([37, Hi, Lo|Rest], Acc) -> % match %Hex 
    unquote(Rest, [(from_hex(Lo) bor (from_hex(Hi) bsl 4))|Acc]);
unquote([$+|Rest], Acc) ->
    unquote(Rest, [$\s|Acc]);
unquote([H|Rest], Acc) ->
    unquote(Rest, [H|Acc]).

%%--------------------------------------------------------------------
%% @spec to_hex(char()) -> hex()
%%
%% @doc convert char to hex code.
%% @end
%%--------------------------------------------------------------------
-spec to_hex(0..16) -> byte().
to_hex(C) when C >= 0, C < 10 -> $0 + C;
to_hex(C) when C >= 0, C < 16 -> $A + (C - 10).

%%--------------------------------------------------------------------
%% @spec from_hex(hex()) -> char()
%%
%% @doc Used to get char from hex code.
%% @end
%%--------------------------------------------------------------------
-spec from_hex(byte()) -> 0..16.
from_hex(C) when C >= $0, C =< $9 -> C - $0;
from_hex(C) when C >= $a, C =< $f -> C - $a + 10;
from_hex(C) when C >= $A, C =< $F -> C - $A + 10.

%%--------------------------------------------------------------------
%% @spec join([string()], Sep::string()) -> string()
%%
%% @doc Joins a list of elements using a separator. 
%% The result is reversed for efficiency.
%% @end
%%--------------------------------------------------------------------
-spec join([string()], string() | char()) -> string().
join(Strings, Sep) ->
    join(Strings, Sep, []).

-spec join([string()], string() | char(), list()) -> string().
join([], _Sep, _Acc) ->
    [];
join([Last], _Sep, Acc) ->
    [Last|Acc];
join([H|Rest], Sep, Acc) ->
    join(Rest, Sep, [Sep, H|Acc]).

-spec nhdr(atom() | binary() | string()) -> string().
nhdr(L) when is_atom(L) ->
    nhdr(atom_to_list(L));
nhdr(L) when is_binary(L) ->
    nhdr(binary_to_list(L));
nhdr(L) when is_list(L) ->
    string:strip(string:to_lower(L)).

normalize_header({K, V}) ->
    {nhdr(K), string:strip(V)}.

%% http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1.2
%% and
%% http://www.ietf.org/rfc/rfc2396.txt, sec 2.4.2
unquote_path(Path) ->
    PathComponents = [unquote(X) || X <- path_components(Path, [], [])],
    lists:flatten(join(PathComponents, "%2F")).

path_components([], Piece, Acc) ->
    [lists:reverse(Piece)|Acc];
path_components("%2f" ++ Rest, Piece, Acc) ->
    path_components(Rest, [], [lists:reverse(Piece)|Acc]);
path_components("%2F" ++ Rest, Piece, Acc) ->
    path_components(Rest, [], [lists:reverse(Piece)|Acc]);
path_components([C|Rest], Piece, Acc) ->
    path_components(Rest, [C|Piece], Acc).

-define(EWGI_SPEC_FIELDS, [{read_input, fun(E, V) -> ?SET_EWGI_READ_INPUT(V, E) end},
                           {write_error, fun(E, V) -> ?SET_EWGI_WRITE_ERROR(V, E) end},
                           {url_scheme, fun(E, V) -> ?SET_EWGI_URL_SCHEME(V, E) end},
                           {version, fun(E, V) -> ?SET_EWGI_VERSION(V, E) end},
                           {data, fun(E, V) -> ?SET_EWGI_DATA(V, E) end}]).

-define(EWGI_HTTP_HEADER_FIELDS, [{http_accept, fun(E, V) -> ?SET_HTTP_ACCEPT(V, E) end},
                                  {http_cookie, fun(E, V) -> ?SET_HTTP_COOKIE(V, E) end},
                                  {http_host, fun(E, V) -> ?SET_HTTP_HOST(V, E) end},
                                  {http_if_modified_since, fun(E, V) -> ?SET_HTTP_IF_MODIFIED_SINCE(V, E) end},
                                  {http_user_agent, fun(E, V) -> ?SET_HTTP_USER_AGENT(V, E) end},
                                  {http_x_http_method_override, fun(E, V) -> ?SET_HTTP_X_HTTP_METHOD_OVERRIDE(V, E) end},
                                  {other, fun(E, V) -> ?SET_HTTP_OTHER(V, E) end}]).

-define(EWGI_REQUEST_FIELDS, [{auth_type, fun(E, V) -> ?SET_AUTH_TYPE(V, E) end},
                              {content_length, fun(E, V) -> ?SET_CONTENT_LENGTH(V, E) end},
                              {content_type, fun(E, V) -> ?SET_CONTENT_TYPE(V, E) end},
                              {ewgi, fun(E, V) -> ?SET_EWGI(V, E) end},
                              {gateway_interface, fun(E, V) -> ?SET_GATEWAY_INTERFACE(V, E) end},
                              {http_headers, fun(E, V) -> ?SET_HTTP_HEADERS(V, E) end},
                              {path_info, fun(E, V) -> ?SET_PATH_INFO(V, E) end},
                              {path_translated, fun(E, V) -> ?SET_PATH_TRANSLATED(V, E) end},
                              {query_string, fun(E, V) -> ?SET_QUERY_STRING(V, E) end},
                              {remote_addr, fun(E, V) -> ?SET_REMOTE_ADDR(V, E) end},
                              {remote_host, fun(E, V) -> ?SET_REMOTE_HOST(V, E) end},
                              {remote_ident, fun(E, V) -> ?SET_REMOTE_IDENT(V, E) end},
                              {remote_user, fun(E, V) -> ?SET_REMOTE_USER(V, E) end},
                              {remote_user_data, fun(E, V) -> ?SET_REMOTE_USER_DATA(V, E) end},
                              {request_method, fun(E, V) -> ?SET_REQUEST_METHOD(V, E) end},
                              {script_name, fun(E, V) -> ?SET_SCRIPT_NAME(V, E) end},
                              {server_name, fun(E, V) -> ?SET_SERVER_NAME(V, E) end},
                              {server_port, fun(E, V) -> ?SET_SERVER_PORT(V, E) end},
                              {server_protocol, fun(E, V) -> ?SET_SERVER_PROTOCOL(V, E) end},
                              {server_software, fun(E, V) -> ?SET_SERVER_SOFTWARE(V, E) end}]).

empty_ewgi_spec() ->
    {'ewgi_spec', undefined, undefined, undefined, undefined,
     gb_trees:empty()}.

empty_http_headers() ->
    {'ewgi_http_headers', undefined, undefined, undefined, undefined,
     undefined, undefined, gb_trees:empty()}.

server_request_foldl(Req0, ParseFun0, ParseEwgiFun, ParseHttpFun) ->
    ParseFun = fun(ewgi, Req) ->
                       request_foldl(Req, ParseEwgiFun, empty_ewgi_spec(), ?EWGI_SPEC_FIELDS);
                  (http_headers, Req) ->
                       request_foldl(Req, ParseHttpFun, empty_http_headers(), ?EWGI_HTTP_HEADER_FIELDS);
                  (Field, Req) ->
                       ParseFun0(Field, Req)
               end,
    request_foldl(Req0, ParseFun, empty_request(), ?EWGI_REQUEST_FIELDS).

request_foldl(Req, ParseFun, EmptyRec, Fields) ->
    lists:foldl(fun({Field, F}, Rec) ->
                        case ParseFun(Field, Req) of
                            undefined ->
                                Rec;
                            V ->
                                F(Rec, V)
                        end
                end, EmptyRec, Fields).

t_lookup_default(K, T, Default) ->
    case gb_trees:lookup(K, T) of
        {value, V} ->
            V;
        none ->
            Default
    end.

t_insert_header(K, Pair, T) ->
    gb_trees:enter(K, lists:reverse([Pair|lists:reverse(t_lookup_default(K, T, []))]), T).

t_enter_header(K, Pair, T) ->
    gb_trees:enter(K, Pair, T).

%% The following method (urlsplit/1) is taken from the MochiWeb
%% project module mochiweb_util.
%% Copyright 2007 MochiMedia, Inc.
%% See LICENSE for more details.

%% @spec urlsplit(Url) -> {Scheme, Netloc, Path, Query, Fragment}
%% @doc Return a 5-tuple, does not expand % escapes. Only supports HTTP style
%%      URLs.
urlsplit(Url) ->
    {Scheme, Url1} = urlsplit_scheme(Url),
    {Netloc, Url2} = urlsplit_netloc(Url1),
    {Path, Query, Fragment} = urlsplit_path(Url2),
    {Scheme, Netloc, Path, Query, Fragment}.

urlsplit_scheme(Url) ->
    urlsplit_scheme(Url, []).

urlsplit_scheme([], Acc) ->
    {"", lists:reverse(Acc)};
urlsplit_scheme(":" ++ Rest, Acc) ->
    {string:to_lower(lists:reverse(Acc)), Rest};
urlsplit_scheme([C | Rest], Acc) ->
    urlsplit_scheme(Rest, [C | Acc]).

urlsplit_netloc("//" ++ Rest) ->
    urlsplit_netloc(Rest, []);
urlsplit_netloc(Path) ->
    {"", Path}.

urlsplit_netloc(Rest=[C | _], Acc) when C =:= $/; C =:= $?; C =:= $# ->
    {lists:reverse(Acc), Rest};
urlsplit_netloc([C | Rest], Acc) ->
    urlsplit_netloc(Rest, [C | Acc]).

%% @spec urlsplit_path(Url) -> {Path, Query, Fragment}
%% @doc Return a 3-tuple, does not expand % escapes. Only supports HTTP style
%%      paths.
urlsplit_path(Path) ->
    urlsplit_path(Path, []).

urlsplit_path("", Acc) ->
    {lists:reverse(Acc), "", ""};
urlsplit_path("?" ++ Rest, Acc) ->
    {Query, Fragment} = urlsplit_query(Rest),
    {lists:reverse(Acc), Query, Fragment};
urlsplit_path("#" ++ Rest, Acc) ->
    {lists:reverse(Acc), "", Rest};
urlsplit_path([C | Rest], Acc) ->
    urlsplit_path(Rest, [C | Acc]).

urlsplit_query(Query) ->
    urlsplit_query(Query, []).

urlsplit_query("", Acc) ->
    {lists:reverse(Acc), ""};
urlsplit_query("#" ++ Rest, Acc) ->
    {lists:reverse(Acc), Rest};
urlsplit_query([C | Rest], Acc) ->
    urlsplit_query(Rest, [C | Acc]).
