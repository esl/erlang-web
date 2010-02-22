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
%%% File	: e_multipart_inets.erl
%%% Author	: Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% Description : Module responsible for INETS multipart 
%%% @hidden
%%%-------------------------------------------------------------------

-module(e_multipart_inets).

-export([get_multipart/2, terminate/0, terminate/1]).

get_multipart(Body, Boundary) ->
    Regexp = Boundary ++ "((\r\n)|(\-\-\r\n))",
    {ok, Split} = regexp:split(Body, Regexp),
    retrive_data(Split).

retrive_data([]) ->
    [];
retrive_data([[] | Rest]) ->
    retrive_data(Rest);
retrive_data(["Content-Disposition: form-data; " ++ Element | Rest]) ->
    {match, HeaderStart, HeaderLen} = regexp:first_match(Element, ".*\r\n"),
    HeaderString = string:substr(Element, HeaderStart, HeaderLen-2),
    {ok, Fields} = regexp:split(HeaderString, "; "),
    Header = lists:foldl(fun(E, Acc) ->
				 {ok, [Key, Value]} = regexp:split(E, "="),
				 [{list_to_atom(Key), Value} | Acc]
			 end, [], Fields),
    HeaderLength = HeaderStart+HeaderLen+2,

    {value, {name, SName}} = lists:keysearch(name, 1, Header),
%% 34 == "
    Name = string:strip(SName, both, 34),
    Content = string:substr(Element, HeaderLength, 
			    length(Element)-1-HeaderLength),

    case e_conf:get_conf(upload_to_disk, true) of
	false ->
	    [{Name, Content} | retrive_data(Rest)];
	true  ->
	    case lists:keysearch(filename, 1, Header) of
		{value, {filename, "\"\""}} ->
		    [{Name, []} | retrive_data(Rest)];
		{value, {filename, SFilename}} ->
		    Filename0 = string:strip(SFilename, both, 34),
		    Filename = case regexp:first_match(Content, "\r\n\r\n") of
				   {match, ContentS, _} ->
				       save_file(Filename0, string:substr(Content, ContentS+4));
				   nomatch ->
				       save_file(Filename0, Content)
			       end,
		    [{Name, Filename} | retrive_data(Rest)];
		false ->
		    [{Name, Content} | retrive_data(Rest)]
	    end
    end.

save_file(SFilename, Content) ->
    BaseDir = dirname(),
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

    Filename = filename:join([BaseDir, SFilename]),
    
    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    file:write(Fd, list_to_binary(Content)),
	    file:close(Fd);
	{error, Reason} ->
	    error_logger:error_msg("~p module, create temporary upload file ~p failed, reason: ~p~n", 
				   [?MODULE, Filename, Reason])
    end,
    
    Filename.

dirname() ->
    filename:join([e_conf:upload_dir(), pid_to_list(self())]).

terminate() ->
    Dirname = dirname(),
    lists:foreach(fun file:delete/1, 
		  filelib:wildcard(filename:join(Dirname, "*"))),
    file:del_dir(Dirname).

terminate(Pid) ->
    Dirname = filename:join([e_conf:upload_dir(), pid_to_list(Pid)]),
    lists:foreach(fun file:delete/1, 
		  filelib:wildcard(filename:join(Dirname, "*"))),
    file:del_dir(Dirname).
