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
-module(wpart_multiselect).
-behaviour(wpart).

-export([handle_call/1, build_html_tag/4, build_html_tag/3, load_tpl/0]).

-deprecated([build_html_tag/4]).

-include_lib("xmerl/include/xmerl.hrl").

-spec(handle_call/1 :: (tuple()) -> tuple()).	     
handle_call(#xmlElement{attributes = Attrs0}) ->
    Attrs = wpart:xml2proplist(Attrs0),

    Selected0 = proplists:get_value("selected", Attrs, ""),
    Selected = lists:map(fun string:strip/1, string:tokens(Selected0, [$|])),

    #xmlText{value=get_html_tag([{"preselected", Selected} | proplists:delete("selected", Attrs)], ""),
	     type=cdata}.

-spec(build_html_tag/3 :: (string(), list(tuple()), list()) -> string()).	     
build_html_tag(Id, Params, Default) ->
    Options = proplists:get_value(options, Params, []),
    Selected = if
		   Default == [] ->
		       proplists:get_value(selected, Params, []);
		   true ->
		       []
	       end,
    Attrs0 = wpart:normalize_html_attrs([{"options", Options},
					 {"preselected", Selected} |
					 proplists:get_value(html_attrs, Params, [])]),
    Attrs = [{"name", Id}, {"id", Id} | proplists:delete("name", Attrs0)],

    get_html_tag(Attrs, Default).

build_html_tag(Name, Prefix, Params, Default) ->
    N = wpart_derived:generate_long_name(Prefix, Name),
    Description = wpart_derived:get_description(Name, Params),
    D = wpart_derived:find(N, Default),
    Attrs0 = wpart:normalize_html_attrs(proplists:get_value(html_tags, Params, [])),
    Attrs = [{"name", N} | proplists:delete("name", Attrs0)],

    wpart_derived:surround_with_table(N, get_html_tag(Attrs, D), Description).
		    
get_html_tag(Attrs0, DefaultList) ->
    Multiselect = wpart_gen:tpl_get(multiselect),

    Preselected = proplists:get_value("preselected", Attrs0, []),
    OptionsString = proplists:get_value("options", Attrs0, []),
    Attrs = proplists:delete("preselected", 
			     proplists:delete("options", Attrs0)),

    Builder = fun(Option, Acc) ->
		      [Value, Desc] = string:tokens(Option, [$:]),

		      OptionAttrs = case checked(Value, DefaultList, Preselected) of
					true ->
					    [{"checked", "checked"} | Attrs];
					false ->
					    Attrs
				    end,

		      Acc ++ wpart_gen:build_html(Multiselect, [{"html", wpart:proplist2html(OptionAttrs)},
							 {"value", Value},
							 {"desc", Desc}])
	      end,

    case string:tokens(OptionsString, [$|]) of
	[] ->
	    "No options loaded.";
	Options ->
	    lists:foldl(Builder, "", Options)
    end.

-spec(checked/3 :: (string(), list(string()), list(string())) -> boolean()).
checked(Value, DefaultList, Preselected) ->
    lists:member(Value, DefaultList) orelse
	lists:member(Value, Preselected).

load_tpl() ->
    wpart_gen:load_tpl(multiselect, 
    		       filename:join([code:priv_dir(wparts),"html","multiselect.tpl"])).
