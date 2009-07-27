%%%-------------------------------------------------------------------
%%% File    : ewgi_mochiweb.erl
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
%%% <p>Reference implementation of a MochiWeb EWGI server gateway.</p>
%%%
%%% @end
%%%
%%% Created : 12 Oct 2007 by Filippo Pacini <filippo.pacini@gmail.com>
%%%-------------------------------------------------------------------
-module(ewgi_mochiweb, [Appl]).

%% ewgi callbacks
-export([run/1]).

-include_lib("ewgi.hrl").

-define(EWGI2MOCHI(Err, Hdrs), {element(1, Err), Hdrs, element(2, Err)}).

%%====================================================================
%% ewgi_server callbacks
%%====================================================================
run(MochiReq) ->
    try parse_arg(MochiReq) of
        Req when ?IS_EWGI_REQUEST(Req) ->
            try process_application(ewgi_api:context(Req, ewgi_api:empty_response())) of
                not_found ->
                    MochiReq:not_found();
                Ctx when ?IS_EWGI_CONTEXT(Ctx) ->
                    handle_result(Ctx, MochiReq)
            catch
                _:Reason ->
                    error_logger:error_report(Reason),
                    MochiReq:respond({500, [], "Internal server error"})
            end
    catch
        _:Reason ->
            error_logger:error_report(Reason),
            MochiReq:respond({400, [], "Bad request"})
    end.

%% Chunked response if a nullary function is returned
handle_result(Ctx, Req) ->
    {Code, _} = ewgi_api:response_status(Ctx),
    Headers = ewgi_api:response_headers(Ctx),
    Body = ewgi_api:response_message_body(Ctx),
    handle_result1(Code, Headers, Body, Req).

handle_result1(Code, Headers, F, Req) when is_function(F, 0) ->
    MochiResp = Req:respond({Code, Headers, chunked}),
    handle_stream_result(MochiResp, F());
handle_result1(Code, Headers, L, Req) ->
    Req:respond({Code, Headers, L}).

%% Treat a stream with chunked transfer encoding
handle_stream_result(R, {}) ->
    R:write_chunk([]);
handle_stream_result(R, {[], T}) when is_function(T, 0) ->
    %% Don't prematurely end the stream
    handle_stream_result(R, T());
handle_stream_result(R, {<<>>, T}) when is_function(T, 0) ->
    %% Don't prematurely end the stream
    handle_stream_result(R, T());
handle_stream_result(R, {H, T}) when is_function(T, 0) ->
    R:write_chunk(H),
    handle_stream_result(R, T()).

process_application(Ctx) when is_list(Appl) ->
    Path = ewgi_api:path_info(Ctx),
    process_mount_application(Ctx, Path, find_mount(Appl, Path));
process_application(Ctx) ->
    ewgi_application:run(Appl, Ctx).

process_mount_application(_, _, {not_found, _}) ->
    not_found;
process_mount_application(Ctx0, Path0, {MountPoint, Application}) ->
    Path = case Path0 of
               "*" -> "*";
               _ -> string:substr(Path0, length(MountPoint) + 1)
           end,
    Ctx = ewgi_api:path_info(Path, ewgi_api:script_name(MountPoint, Ctx0)),
    ewgi_application:run(Application, Ctx).

find_mount([], _) ->
    {not_found, fun (_, _) -> not_found end};
find_mount(Mounts, "*") ->
    lists:last(Mounts);
find_mount([{Path, _}=M|_], Path) ->
    M;
find_mount([{Point, _}=M|T], Path) ->
    case string:str(Path, Point ++ "/") of
        1 ->
            M;
        _ ->
            find_mount(T, Path)
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

parse_arg(Req) ->
    ewgi_api:server_request_foldl(Req, fun parse_element/2, fun parse_ewgi_element/2, fun parse_http_header_element/2).

parse_element(auth_type, _Req) ->
    undefined;

parse_element(content_length, Req) ->
    case Req:get_header_value("content-length") of
        undefined -> undefined;
        Length when is_integer(Length) ->
            Length;
        Length when is_list(Length) ->
            list_to_integer(Length)
    end;

parse_element(content_type, Req) ->
    Req:get_header_value("content-type");

parse_element(gateway_interface, _Req) ->
    "EWGI/1.0";

parse_element(path_info, Req) ->
    RawPath = Req:get(raw_path),
    case RawPath of
        RawPath when RawPath =:= '*' ->
            "*";
        RawPath ->
            {_, _, Path, _, _} = mochiweb_util:urlsplit(RawPath),
            ewgi_api:unquote_path(Path)
    end;

%% Used to be:
%% filename:dirname(filename:dirname(code:which(Appl)))++Req:get(path); The
%% problem here is that the application only has a function which acts as the
%% entry point to the application.
parse_element(path_translated, _Req) ->
    undefined;

parse_element(query_string, Req) ->
    RawPath = Req:get(raw_path),
    case RawPath of
        RawPath when RawPath =:= '*' ->
            undefined;
        RawPath ->
            {_, _, _, QueryString, _} = mochiweb_util:urlsplit(RawPath),
            QueryString
    end;

parse_element(remote_addr, Req) ->
    Req:get(peer);

parse_element(remote_host, _Req) ->
    undefined;

parse_element(remote_ident, _Req) ->
    undefined;

parse_element(remote_user, _Req) ->
    undefined;

parse_element(request_method, Req) ->
    Req:get(method);

%% Default value is empty string. If mount points are used, SCRIPT_NAME
%% becomes the mount point.
parse_element(script_name, _Req) ->
    [];

parse_element(server_name, Req) ->
    HostPort = Req:get_header_value(host),
    case HostPort of
        HostPort when is_list(HostPort) ->
            hd(string:tokens(HostPort, ":"));
        HostPort -> HostPort
    end;

parse_element(server_port, Req) ->
    HostPort = Req:get_header_value(host),
    case HostPort of
        HostPort when is_list(HostPort) ->
            case length(HostPort) of
                2 -> lists:nth(2, HostPort);
                _ -> undefined
            end;
        _ ->
            undefined
    end;

parse_element(server_protocol, Req) ->
    {Maj, Min} = Req:get(version),
    lists:flatten(io_lib:format("HTTP/~b.~b", [Maj, Min]));

parse_element(server_software, _Req) ->
    "MochiWeb";

%% All other elements are undefined
parse_element(_, _) ->
    undefined.

parse_ewgi_element(read_input, Req) ->
    F = fun(Callback, Length) ->
                case Req:get_header_value("expect") of
                    "100-continue" ->
                        Req:start_raw_response({100, gb_trees:empty()});
                    _Else ->
                        ok
                end,
                read_input(Callback, Length, Req)
        end,
    F;

parse_ewgi_element(write_error, Req) ->
    F = fun(Msg) ->
                write_error(Msg, Req)
        end,
    F;

%% https?
parse_ewgi_element(url_scheme, _Req) ->
    "http";

parse_ewgi_element(version, _Req) ->
    {1, 0};

parse_ewgi_element(data, _Req) ->
    gb_trees:empty();

%% Ignore others
parse_ewgi_element(_, _) ->
    undefined.

parse_http_header_element(http_accept, Req) ->
    Req:get_header_value("accept");

parse_http_header_element(http_cookie, Req) ->
    Req:get_header_value("cookie");

parse_http_header_element(http_host, Req) ->
    Req:get_header_value("host");

parse_http_header_element(http_if_modified_since, Req) ->
    Req:get_header_value("if-modified-since");

parse_http_header_element(http_user_agent, Req) ->
    Req:get_header_value("user-agent");

parse_http_header_element(http_x_http_method_override, Req) ->
    Req:get_header_value("x-http-method-override");

parse_http_header_element(other, Req) ->
    lists:foldl(fun({K0, _}=Pair, Acc) ->
                        {K, V} = ewgi_api:normalize_header(Pair),
                        case K of
                            K when K =:= "content-length"
                            ; K =:= "content-type"
                            ; K =:= "accept"
                            ; K =:= "cookie"
                            ; K =:= "host"
                            ; K =:= "if-modified-since"
                            ; K =:= "user-agent"
                            ; K =:= "x-http-method-override" ->
                                Acc;
                            _ ->
                                Ex = case gb_trees:lookup(K, Acc) of
                                         {value, L} ->
                                             L;
                                         none ->
                                             []
                                     end,
                                gb_trees:insert(K, [{K0, V}|Ex], Acc)
                        end
                end, gb_trees:empty(), mochiweb_headers:to_list(Req:get(headers)));

parse_http_header_element(_, _) ->
    undefined.

%% No chunk size specified, so use default
read_input(Callback, Length, Req) when is_integer(Length) ->
    read_input(Callback, {Length, ?DEFAULT_CHUNKSIZE}, Req);

%% Final callback after entire input has been read
read_input(Callback, {Length, _ChunkSz}, _Req) when is_function(Callback), Length =< 0 ->
    Callback(eof);

%% Continue reading and calling back with each chunk of data
read_input(Callback, {Length, ChunkSz}, Req) when is_function(Callback) ->
    Bin = recv_input(Req, Length, ChunkSz),
    Rem = Length - size(Bin),
    NewCallback = Callback({data, Bin}),
    read_input(NewCallback, {Rem, ChunkSz}, Req).

%% Read either Length bytes or ChunkSz, whichever is smaller
recv_input(Req, Length, ChunkSz) when Length > 0, Length < ChunkSz ->
    Req:recv(Length);
recv_input(Req, _, ChunkSz) ->
    Req:recv(ChunkSz).

%% Write errors to error_logger
write_error(Msg, Req) ->
    error_logger:error_report([{message, Msg}, {request, Req}]).
