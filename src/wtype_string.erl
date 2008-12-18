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

-module(wtype_string).
-behaviour(wtype).

-author("support@erlang-consulting.com").
-copyright("Erlang Training & Consulting Ltd.").
-vsn("$Rev").

-include_lib("xmerl/include/xmerl.hrl").
%% API
-export([handle_call/2,validate/1]).

handle_call(_Format, #xmlText{value=String}) ->
    #xmlText{value=String};
handle_call(_Format, String) when is_list(String) ->
    String.

%TODO: handle optional in rest of types. This one is the only one for now.
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

validate({Types,String}) when is_list(String) -> 
    case wpart_valid:is_private(Types) of
	true ->
	    {ok, String};
	false ->
	    case check_min_length(String, Types) of
		{ok, String} ->
		    case check_max_length(String, Types) of
			{ok, String} -> 
			    check_html(String, Types);
			ErrorMax -> 
			    ErrorMax
		    end;
		ErrorMin -> ErrorMin
	    end
    end.

check_min_length(String, Types) ->
    case lists:keysearch(min_length, 1, Types) of
	{value, {min_length, Min}} ->
            N = utf8_api:ulength(String),
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
            N = utf8_api:ulength(String),
	    if
		N > Max ->
		    {error, {too_long, String}};
		true -> 
		    {ok, String}
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
