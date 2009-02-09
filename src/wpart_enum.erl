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
-module(wpart_enum).
-behaviour(wpart).

-export([handle_call/1, build_html_tag/4, load_tpl/0]).

-include_lib("xmerl/include/xmerl.hrl").

handle_call(E) ->
    Name = attribute_getter("name", "no_name_enum", E),
    Choices = attribute_getter("choices", "", E),
    Class = attribute_getter("class", "", E),
    
    #xmlText{value=get_html_tag(Name, Class, Choices, ""),
	     type=cdata}.

build_html_tag(Name, Prefix, Params, Default) ->
    N = wpart_derived:generate_long_name(Prefix, Name),
    Description = wpart_derived:get_description(Name, Params),
    Choices = case lists:keysearch(choices, 1, Params) of
		  false -> [];
		  {value, {choices, List}} -> List
	      end,
    D = wpart_derived:find(N, Default),
    Class = proplists:get_value(class, Params, ""),
    wpart_derived:surround_with_table(N, get_html_tag(N, Class, Choices, D), 
				      Description).
		    
attribute_getter(Name, Default, E) ->
    case wpartlib:has_attribute("attribute::" ++ Name, E) of
	false -> Default;
	Val -> Val
    end.

%% tag should looks like:
%% <wpart:enum name="language" choices="english:English,my:My\ Lang"\>
get_html_tag(Name, Class, ChoicesString, Default) ->
    [{_, Part}] = ets:lookup(templates, {wpart, enum}),

    Inserter = fun(String, Acc) ->
		       {ok, [Value, Desc]} = regexp:split(String, ":"),
		       Checked = if
				     Value == Default -> 
					 "checked=\"checked\"";
				     true -> ""
				 end,
		       Acc ++ wpart_gen:build_html(Part, [Name, Class, Value, Checked, Desc])
	       end,

    {ok, Choices} =  regexp:split(ChoicesString, "|"),
    case Choices of 
	[[]] -> "No choices loaded.";
	_ -> lists:foldl(Inserter, "", Choices)
    end.

load_tpl() ->
    wpart_gen:load_tpl(enum,
		       filename:join([code:priv_dir(wparts),"html","enum.tpl"])).
