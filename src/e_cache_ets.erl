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
%%% File    : e_cache_ets.erl
%%% @author Michal Ptaszek <info@erlang-consulting.com>
%%% @doc Module responsible for managing the ets cached XML records.
%%% @end
%%%-------------------------------------------------------------------
-module(e_cache_ets).

-export([read_file/2, install/0]).
-export([flush/0]).

%% @hidden
install() ->
    ets:new(?MODULE, [named_table, public]).

%%
%% @spec read_file(Filename :: string(), Expander :: atom()) -> term()
%% @doc Reads the file from ets cache.
%% If the file has not been cached or the original one has changed, 
%% the cache will be read once again.<br/>
%% When the specified file is missing 
%% the <b>erlang:error({Reason, File})</b> is called.
%% @end
%%
-spec(read_file/2 :: (string(), atom()) -> term()). 
read_file(File, Exapnder) ->
    Filename = case filelib:is_file(File) of
		   true ->
		       File;
		   false ->
		       filename:join([e_conf:template_root(), File])
	       end,

    case valid_cache(Filename) of
	false ->
	    cache(Filename, Expander);
	CXML ->
	    binary_to_term(CXML)
    end.

%%
%% @spec flush() -> true
%% @doc Removes all the cache entries.
%% Simply invalidates all the cached items.
%% @end
%%
-spec(flush/0 :: () -> true).
flush() ->	     
    ets:delete_all_objects(?MODULE).

-spec(valid_cache/1 :: (string()) -> false | binary()).	     
valid_cache(File) ->
    case ets:lookup(?MODULE, File) of
	[{_, Stamp, CXML}] ->
	    case filelib:last_modified(File) > Stamp of
		true ->
		    false;
		false ->
		    CXML
	    end;
	[] ->
	    false
    end.

-spec(cache/2 :: (string(), atom()) -> term()).	     
cache(File, wpart_xs) ->
    XML = case xmerl_scan:file(File, []) of
	      {error, Reason} ->
		  erlang:error(Reason);
	      {XML3, _} ->
		  XML3
	  end,
    
    ets:insert(?MODULE, {File, {date(), time()}, term_to_binary(XML)}),
    
    XML.
