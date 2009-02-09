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
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_date).
-behaviour(wpart).

-export([handle_call/1, build_html_tag/4, load_tpl/0]).

-include_lib("xmerl/include/xmerl.hrl").

handle_call(E) ->
    Name = wpartlib:attribute("attribute::name", "no_name_date", E),
    Class = wpartlib:attribute("attribute::class", "", E),
    
    #xmlText{value=get_html_tag(Name, Class, "", []),
	     type=cdata}.

build_html_tag(Name, Prefix, Params, Default) ->
    Description = wpart_derived:get_description(Name, Params),
    N = wpart_derived:generate_long_name(Prefix, Name),
    D = wpart_derived:find(N, Default),
    Class = proplists:get_value(class, Params, ""),
    wpart_derived:surround_with_table(N, get_html_tag(N, Class, D, Params), Description).

%% TODO - fetch date format from params!!
get_html_tag(Name, Class, Default, Params) ->
    Date = if
	       Default == "" -> 
		   "";
	       true -> 
		   case lists:keysearch(format, 1, Params) of
		       {value, {_, Format}} ->
			   wtype_date:get_date(Format, Default);
		       _ ->
			   wtype_date:get_date("YYYY-MM-DD", Default)
		   end
	   end,

    [{_,Parts}] = ets:lookup(templates, {wpart, date}),
    wpart_gen:build_html(Parts, [Name, Class, Date]).

load_tpl() ->
    wpart_gen:load_tpl(date, 
		       filename:join([code:priv_dir(wparts),"html","date.tpl"])).
