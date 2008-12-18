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
-module(wpart_datetime).
-behaviour(wpart).

-export([handle_call/1, build_html_tag/4, load_tpl/0]).

-include_lib("xmerl/include/xmerl.hrl").

handle_call(E) ->
    Name = case wpartlib:has_attribute("attribute::name", E) of
	       false -> "no_name_datetime";
	       Val -> Val
	   end,
    
    #xmlText{value=get_html_tag(Name, {date(), time()}),
	     type=cdata}.

build_html_tag(Name, Prefix, Params, Default) ->
    Description = wpart_derived:get_description(Name, Params),
    N = wpart_derived:generate_long_name(Prefix, Name),
    D = wpart_derived:find(N, Default),
    wpart_derived:surround_with_table(N, get_html_tag(N, D), Description).

get_html_tag(Name, Default) ->
    DateTime = if
	           Default == "" -> "";
	           true ->  wtype_datetime:format("Stamp",Default)
	       end,
    
    [{_, Parts}] = ets:lookup(templates, {wpart, datetime}),
    wpart_gen:build_html(Parts, [Name, DateTime]).

load_tpl() ->
    wpart_gen:load_tpl(datetime,
		       filename:join([code:priv_dir(wparts),"html","datetime.tpl"])).
