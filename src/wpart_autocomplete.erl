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
%%% @version $Rev$
%%% @author Michal Zajda <info@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_autocomplete).
-behaviour(wpart).

-export([handle_call/1, build_html_tag/4, load_tpl/0]).

-include_lib("xmerl/include/xmerl.hrl").

%% @doc wpart:autocomplete complete:"something1,something2"
%% generates javascript function to talk with jquery.js and jquery.autocomplete.js
%% (not included in the head section by this wpart!).

handle_call(E) ->
    Name = attribute_getter("name", "no_name_auto", E),
    Complete = attribute_getter("complete", "", E),

    #xmlText{value=get_html_tag(Name, Complete, ""),
	     type=cdata}.

build_html_tag(Name, Prefix, Params, Default) ->
    N = wpart_derived:generate_long_name(Prefix, Name),
    Description = wpart_derived:get_description(Name, Params),
    Complete = case lists:keysearch(complete, 1, Params) of
		  false -> [];
		  {value, {complete, List}} -> List
	      end,
    D = wpart_derived:find(N, Default),
    wpart_derived:surround_with_table(N, get_html_tag(N,Complete,D),Description).
		    
attribute_getter(Name, Default, E) ->
    case wpartlib:has_attribute("attribute::" ++ Name, E) of
	false -> Default;
	Val -> Val
    end.

get_html_tag(Name, Complete, Default) ->
    Result = if 
		 Complete =/= "" ->
		     Args = string:tokens(Complete, "|"),
		     tl(lists:foldl(fun(X,Acc) -> Acc ++ ",'" ++ X ++ "'" end, "", Args));
		 true -> ""
	     end,

    [{_, Parts1}] = ets:lookup(templates, {wpart, autocomplete}),
    [{_, Parts2}] = ets:lookup(templates, {wpart, autocomplete_input_id}),
    wpart_gen:build_html(Parts1, [Name, Name, Result]) ++
	wpart_gen:build_html(Parts2, [Name, Name, Default]).

load_tpl() ->
    {ok, Binary} = file:read_file(filename:join([code:priv_dir(wparts),"html","autocomplete.tpl"])),
    {ok, List} = regexp:split(binary_to_list(Binary), "<% *slot *%>"),
    
    ets:insert(templates, {{wpart, autocomplete}, List}),

    {ok, Binary1} = file:read_file(filename:join([code:priv_dir(wparts),"html","input_id.tpl"])),
    {ok, List1} = regexp:split(binary_to_list(Binary1), "<% *slot *%>"),
    
    ets:insert(templates, {{wpart, autocomplete_input_id}, List1}).
