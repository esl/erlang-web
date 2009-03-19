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
%%% @author Michal Ptaszek <info@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wtype_upload).
-behaviour(wtype).

-export([handle_call/2, validate/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").

handle_call(_Format, XML) -> 
    XML.

-spec(validate/1 :: ({list(), undefined | string()}) -> {ok, undefined | string()} | {error, {bad_extension | too_big, string()}}).	     
validate({Types, undefined}) ->
    case wpart_valid:is_private(Types) of
	true -> {ok, undefined};
        false ->
            case lists:keysearch(optional, 1, Types) of
		{value, {optional, Default}} -> 
                    {ok, Default};
		_ ->  
                    {ok, undefined}
            end
    end;
validate({_Types, []}) ->
    {ok, undefined};
validate({Types, File}) -> 
    case wpart_valid:is_private(Types) of
	true ->
	    {ok, File};
	false ->
	    case check_extension(Types, File) of
		{ok, File} ->
		    check_max_size(Types, File);
		ErrorExt ->
		    ErrorExt
	    end
    end.

-spec(check_extension/2 :: (list(), string()) -> {ok, string()} | {error, {bad_extension, string()}}).
check_extension(Types, File) ->
    case lists:keysearch(extensions, 1, Types) of
	{value, {extensions, Exts}} ->
	    case lists:any(fun(Ext) ->
				   lists:suffix(string:to_lower(Ext), 
						string:to_lower(File))
			   end, Exts) of
		true ->
		    {ok, File};
		false ->
		    {error, {bad_extension, File}}
	    end;
	false ->
	    {ok, File}
    end.

-spec(check_max_size/2 :: (list(), string()) -> {ok, string()} | {error, {too_big, string()}}).	     
check_max_size(Types, File) ->
    case lists:keysearch(max_size, 1, Types) of
	{value, {max_size, Size}} ->
	    {ok, FileInfo} = file:read_file_info(File),
	    CSize = convert_size(Size),
	    if
		FileInfo#file_info.size > CSize ->
		    {error, {too_big, File}};
		true ->
		    {ok, File}
	    end;		    
	false ->
	    {ok, File}
    end.

-spec(convert_size/1 :: (integer() | {integer(), atom()}) -> integer()).
convert_size({Size, bytes}) ->
    Size;
convert_size({Size, kbytes}) ->
    Size bsl 10;
convert_size({Size, mbytes}) ->
    Size bsl 20;
convert_size(Size) ->
    Size.
