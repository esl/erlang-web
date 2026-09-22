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
%%% @author Michal Zajda <info@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_multilist).
-behaviour(wpart).

-export([handle_call/1, build_html_tag/4, build_html_tag/3, load_tpl/0]).

-deprecated([build_html_tag/4]).

-include_lib("xmerl/include/xmerl.hrl").

handle_call(#xmlElement{attributes = Attrs0}) ->
    Attrs = wpart:xml2proplist(Attrs0),

    Selected0 = proplists:get_value("selected", Attrs, ""),
    Selected = lists:map(fun string:strip/1, string:tokens(Selected0, [$|])),
    
    #xmlText{value=get_html_tag([{"preselected", Selected} | proplists:delete("selected", Attrs)], ""),
	     type=cdata}.

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

    D = case Default of
	    Integer when is_integer(Integer) ->
		[integer_to_list(Integer)];
	    _ ->
		Default
	end,
    get_html_tag(Attrs, D).

build_html_tag(Name, Prefix, Params, Default) ->
    N = wpart_derived:generate_long_name(Prefix, Name),
    Description = wpart_derived:get_description(Name, Params),
    D = case wpart_derived:find(N, Default) of
            [] ->
                [];
            [[_|_]|_] = ListOfLists ->
                ListOfLists;
            [Integer|_] = ListofIntegers when is_integer(Integer) ->
                lists:map(fun integer_to_list/1, ListofIntegers);
            Integer when is_integer(Integer) ->
                [integer_to_list(Integer)]
        end,
    Options = proplists:get_value(options, Params, []),
    Selected = if
		   D == [] ->
		       proplists:get_value(selected, Params, []);
		   true ->
		       []
	       end,
    Attrs0 = wpart:normalize_html_attrs([{"options", Options},
					 {"preselected", Selected} | 
					 proplists:get_value(html_attrs, Params, [])]),
    Attrs = [{"name", N} | proplists:delete("name", Attrs0)],

    wpart_derived:surround_with_table(N, get_html_tag(Attrs, D),
				      Description).
		    
get_html_tag(Attrs0, DefaultList) ->
    SelectTpl = wpart_gen:tpl_get(multilist_select),
    OptionTpl = wpart_gen:tpl_get(multilist_option),

    Preselected = proplists:get_value("preselected", Attrs0, []),
    OptionsString = proplists:get_value("options", Attrs0, []),
    Multiple = proplists:get_value("multiple", Attrs0),
    Attrs = proplists:delete("multiple", 
			     proplists:delete("options", 
					      proplists:delete("preselected", Attrs0))),
    
    Inserter = fun(String, Acc) ->
		       [Value, Desc] = string:tokens(String, [$:]),

		       OptionAttrs = case selected(Value, DefaultList, Preselected) of
					 true ->
					     [{"selected", "selected"} | Attrs];
					 false ->
					     Attrs
				     end,

		       Acc ++ wpart_gen:build_html(OptionTpl, [{"html", wpart:proplist2html(OptionAttrs)},
							       {"value", Value},
							       {"desc", Desc}])
	       end,

    case string:tokens(OptionsString, [$|]) of
        [] -> 
	    "No options loaded.";
        Options -> 
	    HtmlOpt = lists:foldl(Inserter, "", Options),
	    SelectAttrs = if
			      Multiple == undefined ->
				  Attrs;
			      true ->
				  [{"multiple", "multiple"} | Attrs]
			  end,

	    wpart_gen:build_html(SelectTpl, [{"html", wpart:proplist2html(SelectAttrs)},
					     {"options", HtmlOpt}])
    end.

selected(Value, DefaultList, Preselected) ->
    lists:member(Value, DefaultList) orelse
    	lists:member(Value, Preselected).

load_tpl() ->
    wpart_gen:load_tpl(multilist_select,
		       filename:join([code:priv_dir(wparts),"html","select.tpl"])),
    wpart_gen:load_tpl(multilist_option,
		       filename:join([code:priv_dir(wparts),"html","option.tpl"])).
