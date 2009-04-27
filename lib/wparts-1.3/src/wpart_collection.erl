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
%% Ltd. Portions created by Erlang Training & Consulting Ltd are Copyright 2009,
%% Erlang Training & Consulting Ltd. All Rights Reserved.

%%%-------------------------------------------------------------------
%%% @version 1.0
%%% @author Michal Ptaszek <info@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_collection).
%-behaviour(wpart).

-export([handle_call/1, build_html_tag/4, load_tpl/0]).

-include_lib("xmerl/include/xmerl.hrl").

handle_call(E) ->
    Name = case wpartlib:has_attribute("attribute::name", E) of
	       false -> "no_name_collection";
	       Val -> Val
	   end,
    Type = wpartlib:has_attribute("attribute::type", E),
    Extra = case wpartlib:has_attribute("attribute::extra", E) of
		false -> "";
		Ex -> Ex
	    end,
    
    #xmlText{value=get_html_tag(Name, [], Type, Extra),
	     type=cdata}.

%% @todo add extra params?
build_html_tag(Name, Prefix, Params, Default) ->
    Description = wpart_derived:get_description(Name, Params),
    N = wpart_derived:generate_long_name(Prefix, Name),
    D = wpart_derived:find(N, Default),
    Type = atom_to_list(element(1, proplists:get_value(type, Params))),

    wpart_derived:surround_with_table(N, get_html_tag(N, D, Type, ""), Description).

%% @todo handle default
get_html_tag(Name, Default, Type, Extra) ->
    TypeMod = list_to_atom("wpart_" ++ Type),
    Elements = lists:foldl(fun({ElementName, _} = Def, Acc) ->
				   Row0 = TypeMod:build_html_tag(list_to_atom(ElementName), "", [{description , ""}], [Def]),
				   Row = get_content(Row0),
				   
				   [Row | Acc]
			   end, [], Default),
    Counter = integer_to_list(length(Default)+1),

    InputElement0 = TypeMod:build_html_tag(list_to_atom(Name ++ "_" ++ Counter), "", [{description, ""}], []),
    InputElement = get_content(InputElement0),
    
    HTML = wpart_gen:build_html(wpart_gen:tpl_get(collection), [{"name", Name},
								{"extra", Extra},
								{"type", Type},
								{"input_element", InputElement},
								{"editing_elements", lists:flatten(Elements)},
								{"counter_start", Counter}]),

    {XML, _} = xmerl_scan:string(HTML),
    element(2, regexp:gsub(lists:flatten(wpart_xs:template(XML)), "\n", "")).

get_content(String) ->
    {XML, _} = xmerl_scan:string(String),
    Input = xmerl_xpath:string("td[2]//node()", XML),

    lists:flatten(wpart_xs:template(Input)).

load_tpl() ->
    wpart_gen:load_tpl(collection,
		       filename:join([code:priv_dir(wparts),"html","collection_element.tpl"])).
