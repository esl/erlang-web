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
%%% File    : s_multipart.erl
%%% Author  : Martin <info@erlang-consulting.com>
%%% Description : Multipart handling
%%% @hidden
%%%-------------------------------------------------------------------
-module(e_multipart_yaws).

%% API
-export([terminate/0, parse/1, is_multipart/1, terminate/1]).

-include_lib("yaws/include/yaws_api.hrl").

%%====================================================================
%% API
%%====================================================================

terminate() ->
    e_multipart_common:delete_tmp_files(self()).

terminate(Pid) ->
    e_multipart_common:delete_tmp_files(Pid).

is_multipart(#arg{req = R, headers = H}) ->
    case {R#http_request.method, H#headers.content_type} of
	{'POST', "multipart" ++ _} -> true;
	_                          -> false
    end.

%%--------------------------------------------------------------------
%% Function:
%% Description:
%%--------------------------------------------------------------------
parse(#arg{} = A) when A#arg.state == undefined ->
    parse(A#arg{state = {[],[]}});
parse(A) when is_record(A, arg) ->
    case yaws_api:parse_multipart_post(A) of
	{cont, Cont, Res} ->
	    {get_more, Cont, add_chunk(Res, A#arg.state)};
	{result, Res} ->
            case e_conf:get_conf(upload_to_disk, true) of
                false -> Handler = add_body(Res,[]),
                         {ok, Handler};
                _ -> Handler = add_chunk(Res, A#arg.state),
	    %% we cast {file, Filename} to Filename for
	    %% compatibility with Inets' multipart
                  {ok, lists:map(fun({Key, {file, Val}}) ->
                                            {Key, Val};
                                       (Else) ->
                                            Else
                                    end, element(2, Handler))}
            end
    end.
%%====================================================================
%% Internal functions
%%====================================================================
% head {"this_is_file",[{filename,"review.log"},{name,"this_is_file"}]}
add_body([{head, {Name, Opt}}|Res], SoFar) ->
    add_body(Res, {Name, SoFar});
add_body([{body, Data}|Res], {Name, SoFar}) ->
    if
	length(Data) == 0 ->
	    add_body(Res, lists:append(SoFar, [{Name, []}]));
	true ->
	    add_body(Res, lists:append(SoFar, [{Name, Data}]))
    end;
add_body([], State) ->
    State.


%%====================================================================
%% Internal functions
%%====================================================================
% head {"this_is_file",[{filename,"review.log"},{name,"this_is_file"}]}
add_chunk([{head, {Name, Opt}}|Res], {_, SoFar}) ->
    case lists:keysearch(filename, 1, Opt) of
	{value, {filename, Filename0}} ->
	    BaseDir = e_multipart_common:base_dir(self()),

	    FolderCreator = fun(Element, Acc) ->
				    Next =
					if Acc =/= "" ->
						filename:join(Acc, Element);
					   true ->
						Element
					end,
				    case file:make_dir(Next) of
					ok ->
					    Next;
					{error, eexist} ->
					    Next;
					%% special case of Mac Os - it returns
					%% eisdir instead of eexist
					{error, eisdir} ->
					    Next;
					{error, Reason} ->
					    error_logger:error_msg("~p module, cannot create directory ~p, reason: ~p~n",
								   [?MODULE, Next, Reason]),
					    ""
				    end
			    end,
	    lists:foldl(FolderCreator, "", filename:split(BaseDir)),

	    Filename = filename:join(BaseDir,
				     basename(
				       yaws_api:sanitize_file_name(
					 Filename0))),

	    if
		Filename0 =/= [] ->
		    case file:open(Filename, [append]) of
			{ok, File} ->
			    file:write(File, list_to_binary("")),
			    file:close(File);
			{error, Reason} ->
			    error_logger:error_msg("~p module, error in multipart: ~p~n", [?MODULE, Reason])
		    end;
		true -> true
	    end,

	    add_chunk(Res, {{Name, {file, Filename}}, SoFar});
	false ->
	    add_chunk(Res, {{Name, []}, SoFar})
    end;
add_chunk([{part_body, Data}|Res], {{Name, {file, Filename}}, SoFar}) ->
    append_file(Filename, Data),
    add_chunk(Res, {{Name, {file, Filename}}, SoFar});
add_chunk([{part_body, Data}|Res], {{Name, Chunk}, SoFar}) ->
    add_chunk(Res, {{Name, Chunk ++ Data}, SoFar});
add_chunk([{body, Data}|Res], {{Name, {file, Filename}}, SoFar}) ->
    if
	length(Data) == 0 ->
	    add_chunk(Res, {[], SoFar});
	true ->
	    append_file(Filename, Data),
	    add_chunk(Res, {[], [{Name, {file, Filename}}|SoFar]})
    end;
add_chunk([{body, Data}|Res], {{Name, Chunc}, SoFar}) ->
    add_chunk(Res, {[], [{Name, Chunc ++ Data}|SoFar]});
add_chunk([], State) ->
    State.

basename(FilePath) ->
    case string:rchr(FilePath, $\\) of
	0 ->
	    %% probably not a DOS name
	    filename:basename(FilePath);
	N ->
	    %% probably a DOS name, remove everything after last \
	    basename(string:substr(FilePath, N+1))
    end.

append_file(Filename, Data) ->
    {ok, Ref} = file:open(Filename, [append]),
    ok = file:write(Ref, Data),
    ok = file:close(Ref).
