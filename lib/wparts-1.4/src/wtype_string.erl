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
-module(wtype_string).
-behaviour(wtype).

-include_lib("xmerl/include/xmerl.hrl").

-export([handle_call/2, validate/1]).

handle_call(_Format, #xmlText{value=String}) ->
    #xmlText{value=String};
handle_call(_Format, String) when is_list(String) ->
    String;
handle_call(_Format, undefined) ->
    [].

validate({Types,undefined}) -> 
    case wpart_valid:is_private(Types) of
	true ->
	    {ok, undefined};
        false ->
            case lists:keysearch(optional, 1, Types) of
	       {value, {optional, Default}} -> 
                    {ok, Default};
	       _ ->  
                    {error, {empty_input, undefined}}
            end
    end;

validate({Types,RawString}) when is_list(RawString) -> 
    String = utf8_api:ustring(RawString),
    case wpart_valid:is_private(Types) of
	true ->
	    {ok, String};
	false ->
	    case check_min_length(String, Types) of
		{ok, String} ->
		    case check_max_length(String, Types) of
			{ok, String} -> 
			    case check_regexp(String, Types) of
					{ok, String} ->
					    check_html(String, Types);
					ErrorRegexp ->
				  	  ErrorRegexp
			    end;
			ErrorMax -> 
			    ErrorMax
		    end;
		ErrorMin -> 
		    ErrorMin
	    end
    end.

check_min_length(String, Types) ->
    case lists:keysearch(min_length, 1, Types) of
	{value, {min_length, Min}} ->
            N = length(String),
    	    if
		N < Min ->
		    {error, {too_short, String}};
		true -> 		    
		    {ok, String}
	    end;
	_ -> 
	    {ok, String}
    end.

check_max_length(String, Types) ->
    case lists:keysearch(max_length, 1, Types) of
	{value, {max_length, Max}} ->
            N = length(String),
	    if
		N > Max ->
		    {error, {too_long, String}};
		true -> 
		    {ok, String}
	    end;
	_ -> 
	    {ok, String}
    end.

check_regexp(String, Types) ->
    case lists:keysearch(regexp, 1, Types) of
	{value, {regexp, Regexp}} ->
	    case re:run(String, Regexp) of
		{match, _} ->
		    {ok, String};
		nomatch ->
		    {error, {regexp_does_not_match, String}}
	    end;
	_ ->
	    {ok, String}
    end.

check_html(String, Types) ->
    case lists:keysearch(html, 1, Types) of
	{value, {html, Whitelist}} ->
	    case wtype_html:parse_html(String, Whitelist, ["html"]) of
		ok ->
		    {ok, String};
		{error, Reason} ->
		    {error, {{not_valid_html, Reason}, String}}
	    end;
	_ ->
	    {ok, String}
    end.
