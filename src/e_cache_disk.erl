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
%%% File    : e_cache_disk.erl
%%% @author Martin Carlson <martin@erlang-consulting.com>
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Module responsible for managing the disk cached xmerl records.
%%% @end
%%%-------------------------------------------------------------------
-module(e_cache_disk).

%% API
-export([read_file/2]).

-type(xmlElement() :: term()).

%%====================================================================
%% API
%%====================================================================

%%
%% @spec read_file(Filename :: string(), Expander :: atom()) -> term()
%% @doc Reads the file from disk cache.
%% If the file has not been cached or the original one has changed, 
%% the cache will be read once again.<br/>
%% When the specified file is missing or the cached file cannot be 
%% created, the <b>erlang:error({Reason, File})</b> is called.
%% @end
%%
-spec(read_file/2 :: (string(), atom()) -> xmlElement() | no_return()).	     
read_file(File, Expander) ->
    case valid_cache(File) of
	false ->
	    cache(File, Expander);
	true ->
	    Filename = [e_conf:cache_dir(), "/",
			wpart_lang:get_lang_list(), "/",
			filename:rootname(File) ++ ".cxml"],
	    case file:read_file(Filename) of
		{ok, CXML} ->
		    binary_to_term(CXML);
		Error ->
		    Error
	    end
    end.

%%====================================================================
%% Internal functions
%%====================================================================
-spec(cache/2 :: (string(), atom()) -> xmlElement() | no_return()).   
cache(File, xmerl_xs) ->
    XML = case xmerl_scan:file(File, []) of
	      {error, enoent} ->
		  case xmerl_scan:file(e_conf:template_root() ++ "/" ++ File, []) of
		      {error, Reason} ->
			  erlang:error({Reason, File});
		      {XML2, _} ->
			  XML2
		  end;
	      {error, Reason} ->
		  erlang:error(Reason);
	      {XML3, _} ->
		  XML3
	  end,

    Filename = [e_conf:cache_dir(), "/",
		wpart_lang:get_lang_list(), "/",
		filename:rootname(File) ++ ".cxml"],
    Dirs = filename:split(filename:dirname(Filename)),
    
    lists:foldl(fun create_dirs/2, "", Dirs),
    
    Cached = file:write_file(Filename,
			     term_to_binary(XML)),
    if
	Cached == ok -> 
	    true;
	true -> 
	    error_logger:warning_report({caching_failed, Filename})
    end,
    XML.

-spec(valid_cache/1 :: (string()) -> bool()).
valid_cache(File) ->
    Filename = [e_conf:cache_dir(), "/",
		filename:rootname(File) ++ ".cxml"],
    Cache = filename:rootname(Filename) ++ ".cxml",
    Lang = ["config/", 
	    "languages/", 
	    wpart_lang:get_lang_list() ++ ".conf"],

    filelib:is_file(Cache) andalso 
	filelib:last_modified(Filename) < filelib:last_modified(Cache) andalso
	filelib:is_file(Lang) andalso
	filelib:last_modified(Lang) < filelib:last_modified(Cache).

-spec(create_dirs/2 :: (string(), string()) -> string()).
create_dirs(Element, Acc) ->
    Next = 
	if Acc =/= "" ->
		[Acc, "/", Element];
	   true ->
		Element
	end,
    case file:make_dir(Next) of
	ok ->
	    Next;
	{error, eexist} -> 
	    Next;
	{error, _} ->
	    ""
    end.
