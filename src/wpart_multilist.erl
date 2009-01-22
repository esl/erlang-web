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
-module(wpart_multilist).
-behaviour(wpart).

-export([handle_call/1, build_html_tag/4, load_tpl/0]).

-include_lib("xmerl/include/xmerl.hrl").

%% @doc <wpart:multilist name="bla" multiple="true" options="x1:something1,x2:something2,x3:something3"/>
%% generates argument for html form tag of choice list, with multiple checked it is multi list.
%% Available also with wpart_derived, with proper Something_records.hrl syntax.

handle_call(E) ->
    Name = attribute_getter("name", "no_name_multilist", E),
    Options = attribute_getter("options", "", E),
    Multiple = attribute_getter("multiple", "", E),
    
    #xmlText{value=get_html_tag(Name, Options, Multiple, ""),
	     type=cdata}.

build_html_tag(Name, Prefix, Params, Default) ->
    N = wpart_derived:generate_long_name(Prefix, Name),
    Description = wpart_derived:get_description(Name, Params),
    Options = case lists:keysearch(options, 1, Params) of
		  false -> [];
		  {value, {options, List}} -> List
	      end,
    Multi = case lists:keysearch(multiple, 1, Params) of
		  false -> [];
		  {value, {multiple, List2}} -> List2
	      end,
    D = wpart_derived:find(N, Default),
    wpart_derived:surround_with_table(N, get_html_tag(N,Options,Multi,D), 
				      Description).
		    
attribute_getter(Name, Default, E) ->
    case wpartlib:has_attribute("attribute::" ++ Name, E) of
	false -> Default;
	Val -> Val
    end.

get_html_tag(Name, Opt_list, Multiple, DefaultList) ->
    [{_, PartS}] = ets:lookup(templates, {wpart, multilist_select}),
    [{_, PartO}] = ets:lookup(templates, {wpart, multilist_option}),
    
    Inserter = fun(String, Acc) ->
		       {ok, [Value, Desc]} = regexp:split(String, ":"),
		       Bool = lists:member(Value,DefaultList) orelse Value == DefaultList,
		       Selected = if
				      Bool -> 
					  "selected=\"selected\"";
				      true -> ""
				  end,
		       
		       Acc ++ wpart_gen:build_html(PartO, [Value, Selected, Desc])
	       end,
    MultipleHtml = if 
		       Multiple == "true" -> "multiple=\"multiple\"";
		       true -> ""
		   end,

    {ok, ReadyOpt_list} = regexp:split(Opt_list, "|"),
    case ReadyOpt_list of
        [[]] -> "No options loaded.";
        _ -> HtmlOpt = lists:foldl(Inserter, "", ReadyOpt_list),
             wpart_gen:build_html(PartS, [Name, MultipleHtml, HtmlOpt])
    end.

load_tpl() ->
    {ok, Binary} = file:read_file(filename:join([code:priv_dir(wparts),"html","select.tpl"])),
    {ok, List} = regexp:split(binary_to_list(Binary), "<% *slot *%>"),
    
    ets:insert(templates, {{wpart, multilist_select}, List}),
    
    {ok, Binary1} = file:read_file(filename:join([code:priv_dir(wparts),"html","option.tpl"])),
    {ok, List1} = regexp:split(binary_to_list(Binary1), "<% *slot *%>"),
    
    ets:insert(templates, {{wpart, multilist_option}, List1}).
