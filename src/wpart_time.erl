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
%%% @author Michal Zajda  <info@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_time).
-behaviour(wpart).

-export([handle_call/1, build_html_tag/4, load_tpl/0]).

-include_lib("xmerl/include/xmerl.hrl").

handle_call(#xmlElement{attributes = Attrs0}) ->
    Attrs = wpart:xml2proplist(Attrs0),
    
    #xmlText{value=get_html_tag(Attrs, ""),
	     type=cdata}.

build_html_tag(Name, Prefix, Params, Default) ->
    Description = wpart_derived:get_description(Name, Params),
    N = wpart_derived:generate_long_name(Prefix, Name),
    D = wpart_derived:find(N, Default),

    Attrs0 = wpart:normalize_html_attrs(proplists:get_value(html_attrs, Params, [])),
    Attrs = [{"name", N} | proplists:delete("name", Attrs0)],

    wpart_derived:surround_with_table(N, get_html_tag(Attrs, D), Description).

get_html_tag(Attrs, Default) when is_tuple(Default) ->
    Format = proplists:get_value("format", Attrs, "HH:MM:SS"),
    get_html_tag(Attrs, wtype_time:get_time(Format, Default));
get_html_tag(Attrs, Default) ->
    wpart_gen:build_html(wpart_gen:tpl_get(time),
			 [{"html", wpart:proplist2html(proplists:delete("format", Attrs))},
			  {"value", Default}]).

load_tpl() ->
    wpart_gen:load_tpl(time, 
		       filename:join([code:priv_dir(wparts),"html","time.tpl"])).
