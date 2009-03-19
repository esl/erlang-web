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
-module(wpart_enum).
-behaviour(wpart).

-export([handle_call/1, build_html_tag/4, load_tpl/0]).

-include_lib("xmerl/include/xmerl.hrl").

handle_call(#xmlElement{attributes = Attrs0}) ->
    Attrs = wpart:xml2proplist(Attrs0),

    Chosen = proplists:get_value("chosen", Attrs, ""),
    
    #xmlText{value=get_html_tag(Attrs, Chosen),
	     type=cdata}.

build_html_tag(Name, Prefix, Params, Default) ->
    N = wpart_derived:generate_long_name(Prefix, Name),
    Description = wpart_derived:get_description(Name, Params),
    D = wpart_derived:find(N, Default),
    Chosen = if
		 D == [] ->
		     proplists:get_value(chosen, Params, []);
		 true ->
		     D
	     end,

    Choices = proplists:get_value(choices, Params, ""),
    Attrs0 = wpart:normalize_html_attrs(proplists:get_value(html_attrs, Params, [])),
    Attrs = [{"name", N}, {"choices", Choices} | proplists:delete("name", Attrs0)],

    wpart_derived:surround_with_table(N, get_html_tag(Attrs, Chosen), Description).
		    
get_html_tag(Attrs0, Default) ->
    Enum = wpart_gen:tpl_get(enum),

    ChoicesString = proplists:get_value("choices", Attrs0, ""),
    Attrs = proplists:delete("choices", Attrs0),

    Inserter = fun(String, Acc) ->
		       [Value, Desc] = string:tokens(String, ":"),
		       CheckedAttrs = if
					  Value == Default -> 
					      [{"checked", "checked"} | Attrs];
					  true ->
					      Attrs
				      end,
		       Acc ++ wpart_gen:build_html(Enum, [{"html", wpart:proplist2html(CheckedAttrs)},
							  {"value", Value},
							  {"desc", Desc}])
	       end,

    case string:tokens(ChoicesString, [$|]) of 
	    [] -> "No choices loaded.";
	    Tokens -> lists:foldl(Inserter, "", Tokens)
    end.

load_tpl() ->
    wpart_gen:load_tpl(enum,
		       filename:join([code:priv_dir(wparts),"html","enum.tpl"])).
