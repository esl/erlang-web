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
%%% File    : e_mod_ewgi.erl
%%% @author <info@erlang-consulting.com>
%%% @doc EWGI server callback module.
%%% @end
%%%-------------------------------------------------------------------

-module(e_mod_ewgi).
-export([do/1]).

-include_lib("eptic/include/e_mod_ewgi.hrl").

do(#ewgi_context{request = Request} = Context) ->
    e_logger:register_pid(self()),
    case handle_args(Request) of
        {ok, Args} ->
            [$/ | URL] = Request#ewgi_request.path_info,
            e_dict:init_state(Args),

            e_logger:log({?MODULE, {url, URL}}),

            e_dict:fset("__https", (Request#ewgi_request.ewgi)#ewgi_spec.url_scheme == "https"),
            e_dict:fset("__ip", Request#ewgi_request.remote_addr),

            Headers = parse_headers(Request#ewgi_request.http_headers),
            ClientCookie = e_mod_inets:cookie_up(Headers),

            e_dict:fset("__path", URL),
            e_dict:fset("__cookie_key", ClientCookie),

            ControllerFun = fun() ->
                    case e_mod_gen:handle_request(Request#ewgi_request.path_info) of
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
                {NewHeaders, Body} ->
                    CookieHeader = e_mod_inets:cookie_bind(ClientCookie),

                    e_logger:unregister_pid(self()),
                    {C, H} = case lists:keytake(code, 1, NewHeaders) of
                        {value, {code, Code}, Hdrs} ->
                            {Code, [CookieHeader | Hdrs]};
                        false ->
                            {200, [CookieHeader | NewHeaders]}
                    end,
                    Response = #ewgi_response{
                        status = {C, lists:keysearch(C, 1, ?STATUS_CODES)},
                        message_body = Body,
                        headers = H
                    },
                    e_mod_inets:cleanup(),
                    Context#ewgi_context{response = Response};
                enoent ->
                    e_mod_inets:cookie_bind(ClientCookie),
                    e_mod_inets:cleanup(),

                    e_logger:unregister_pid(self()),
                    not_found
            end
    end.

parse_headers(Headers) ->
    ParsedHeaders = [
        {"accept", Headers#ewgi_http_headers.http_accept},
        {"host", Headers#ewgi_http_headers.http_host},
        {"user-agent", Headers#ewgi_http_headers.http_accept},
        {"cookie", Headers#ewgi_http_headers.http_cookie},
        {"if-modified-since", Headers#ewgi_http_headers.http_if_modified_since},
        {"x-http-method-override", Headers#ewgi_http_headers.http_x_http_method_override}
    ],
    lists:filter(fun({_, V}) -> V =/= undefined end, ParsedHeaders).

read_input_cb(Acc) ->
    fun({data, Bin}) ->
            read_input_cb([Bin | Acc]);
        (eof) ->
            Acc
    end.

handle_args(#ewgi_request{request_method = Method, ewgi = Spec} = Request) ->
    ReadInput = Spec#ewgi_spec.read_input,
    Post = case Request#ewgi_request.content_length of
        undefined ->
            [];
        _ ->
            ContentLength = Request#ewgi_request.content_length,
            Bin = ReadInput(read_input_cb([]), ContentLength),
            binary_to_list(list_to_binary(Bin))
    end,
    Result = case Method of
        'POST' ->
            {ok, [{"get", e_mod_inets:parse_get(Request#ewgi_request.path_info)},
                    {"post", e_mod_inets:parse_post(Post)}]};
        _ ->
            {ok, [{"get", e_mod_inets:parse_get(Request#ewgi_request.path_info)}]}
    end,
    e_logger:log({?MODULE, {handle_args, Result}}),
    Result.

controller_exec(Ret, View) ->
    case Ret of
        template ->
            format_response(e_mod_gen:template(e_mod_gen:template_file(View), [],
                    e_conf:template_expander()));
        {redirect, URL} ->
            {[{code, 302}, {location, URL}, {"Content-length", "0"}], []};
        {content, html, Data} ->
            Length = {"Content-length", integer_to_list(erlang:iolist_size(Data))},
            {[{"Content-type", "text/html"}, {code, 200}, Length], Data};
        {content, xml, Data} ->
            Length = {"Content-length", integer_to_list(erlang:iolist_size(Data))},
            {[{"Content-type", "application/xml"}, {code, 200}, Length], Data};
        {content, text, Data} ->
            Length = {"Content-length", integer_to_list(erlang:iolist_size(Data))},
            {[{"Content-type", "text/plain"}, {code, 200}, Length], Data};
        {content, pdf, Data} ->
            Length = {"Content-length", integer_to_list(erlang:iolist_size(Data))},
            {[{"Content-type", "application/pdf"}, {code, 200}, Length], Data};
        {json, Data} ->
            Content = e_json:encode(Data),
            Length = {"Content-length", integer_to_list(length(Content))},
            {[{"Content-type", "text/plain"}, {code, 200}, Length], Content};
        {template, Template} ->
            format_response(e_mod_gen:template(Template, [],
                    e_conf:template_expander()));
        {custom, Custom} ->
            Custom;
        {headers, Headers, NewRet} ->
            {NewHeaders, ProperRet} = controller_exec(NewRet, View),
            ReadyHeaders = e_mod_inets:create_headers(Headers, []),
            {ReadyHeaders ++ NewHeaders, ProperRet};
        {error, Code} ->
            format_response(e_mod_gen:error_page(Code, e_dict:fget("__path")))
    end.

format_response({html, HTML}) ->
    Length = {"Content-length", integer_to_list(erlang:iolist_size(HTML))},
    {[{"Content-type", "text/html"}, {code, 200}, Length], HTML};
format_response([{status, Code}, {html, HTML}]) ->
    Length = {"Content-length", integer_to_list(erlang:iolist_size(HTML))},
    {[{"Content-type", "text/html"}, {code, Code}, Length], HTML};
format_response(Else) ->
    Else.

with_formatted_error(F) ->
    case catch F() of
        {'EXIT', Reason} ->
            format_response(e_mod_gen:error_page(501, e_dict:fget("__path"), Reason));
        Response ->
            format_response(Response)
    end.

