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
-module(wpart_derived).
-export([handle_call/1]).
-export([generate_long_name/2, get_description/2, 
	 find/2, surround_with_table/3, load_tpl/0]).

-include_lib("xmerl/include/xmerl.hrl").

-spec(handle_call/1 :: (tuple()) -> tuple()).	     
handle_call(E) ->
    case wpartlib:has_attribute("attribute::type", E) of
	false -> 
	    error_logger:error_msg("~p module, error during processing the handle_call/1 function.~n"
				   "wpart:derived tag must have the type attribute~n~n",
				   [?MODULE]),
	    #xmlText{value=""};
	Type -> 
	    FormType = case wpartlib:has_attribute("attribute::form_type", E) of
			   false ->
			       undefined;
			   Else ->
			       list_to_atom(Else)
		       end,
	    Prefix = case wpartlib:has_attribute("attribute::long_name", E) of
			 false -> 
			     "";
			 Long -> 
			     Long ++ "_"
		     end,
	    Inputs = build_tags(Type, FormType, Prefix),
	    
	    Templater = fun(Tag) ->
                                Flatten = "<?xml version=\"1.0\" encoding=\"utf-8\"?><div>" ++ Tag ++ "</div>",
				{XML, _} = xmerl_scan:string(Flatten),
				"<div>" ++ R = lists:flatten(wpart_xs:template(XML)),
				">vid/<" ++ R2 = lists:reverse(R),
				lists:reverse(R2)
			end,
	    Result = string:join(lists:map(Templater, Inputs), "\n"),

	    #xmlText{value=Result,
		     type=cdata}
    end.

check_dict(N) ->
    case wpart:fget(N) of  
        "failed" -> "failed";
        _ -> true
    end.

surround_with_table(Name, Field, Description) ->
    Bool = check_dict(Name),
    Err = if 
	      Bool =/= true -> "form_error";
	      true -> Name
	  end,

    [{_, Parts}] = ets:lookup(templates, {wpart, table_row}),
    wpart_gen:build_html(Parts, [Name, Name, Description, Err, Field]).

-spec(generate_long_name/2 :: (string(), atom()) -> string()).	     
generate_long_name(Prefix, Name) ->
    Prefix ++ atom_to_list(Name).

-spec(get_description/2 :: (term(), list()) -> term()).	     
get_description(Name, Tuples) ->
    get_param(Name, description, Tuples).

-spec(get_description/1 :: (list()) -> string()).	     
get_description(Tuples) ->
    find(description, Tuples).

-spec(get_comment/1 :: (list()) -> string()).
get_comment(Tuples) ->
    find(comment, Tuples).

-spec(get_param/3 :: (term(), atom(), list()) -> term()).
get_param(Name, Param, Params) ->
    case lists:keysearch(Param, 1, Params) of
	false when is_atom(Name) -> 
	    atom_to_list(Name);
	false -> 
	    Name;
	{value, {Param, {key, Key}}} ->
	    wpart_lang:get_translation(Key);
	{value, {Param, Desc}} -> 
	    Desc
    end.

-spec(find/2 :: (term(), list()) -> term()).	     
find(Name, List) ->
    case lists:keysearch(Name, 1, List) of
	{value, {_, undefined}} ->
	    "";
	{value, {_, {key, Key}}} ->
	    wpart_lang:get_translation(Key);
	{value, {_, Val}} ->
	    Val;
	false ->
	    ""
    end.

-spec(build_html_tag/7 :: (atom(), atom(), string(), list(), term(), atom(), list(atom())) -> string()).	     
build_html_tag(Type, Name, Prefix, Params, Default, FormType, Primitives) ->
    case lists:member(Type, Primitives) of
	true ->
	    build_html_tag(FormType, Type, Name, Prefix, Params, Default);
	false ->
	    build_html_tag(Type, Name, Prefix, Params)
    end.

-spec(build_html_tag/6 :: (atom(), atom(), atom(), string(), list(), list()) -> string()).	     
build_html_tag(FormType, Type, Name, Prefix, Params, Defaults) ->
    Module = list_to_atom("wpart_" ++ atom_to_list(Type)),
    LName = generate_long_name(Prefix, Name),
    Input = Module:build_html_tag(LName, Params, find(LName, Defaults)),
    Span = wpart_gen:tpl_get(span),

    Proplist0 = [{"id", LName}, {"input", Input}, {"description", get_description(Params)}],
    Proplist1 = case e_error:description(LName) of
		    [] ->
			Proplist0;
		    ErrorDesc ->
			[{"error", wpart_gen:build_html(Span, [{"content", ErrorDesc},
							       {"id", LName ++ "_error"},
							       {"class", "form_error"}])}
			 | Proplist0]
		end,
    Proplist = case get_comment(Params) of
		   [] ->
		       Proplist1;
		   CommentDesc ->
		       [{"comment", wpart_gen:build_html(Span, [{"content", CommentDesc},
								{"id", LName ++ "_comment"},
								{"class", "form_comment"}])}
			| Proplist1]
	       end,

    wpart_gen:build_html(wpart_gen:tpl_get(form_type(FormType)), 
			 Proplist).

-spec(build_html_tag/4 :: (atom(), atom(), string(), list()) -> string()).	     
build_html_tag(Type, _Name, Prefix, _Params) ->
    ListType = atom_to_list(Type),
    
    wpart_gen:build_html(wpart_gen:tpl_get(derived), [Prefix ++ ListType, 
						      ListType,
						      Prefix ++ ListType]).

-spec(build_tags/3 :: (string(), atom(), string()) -> string()).	     
build_tags(Type, FormType, Prefix) ->
    Default = case wpart:fget("__edit") of
		  undefined -> [];
		  V -> V
	      end,

    Module = list_to_atom("wtype_" ++ Type),
    Fields = apply(Module, get_record_info,
		   [list_to_atom(Type)]),
    [_ | Types] = tuple_to_list(apply(Module, get_record_info,
				      [list_to_atom(Type ++ "_types")])),
    FormOptions = (catch Module:get_record_info(list_to_atom(Type ++ "_form"))),
    %% @todo handle the form options, such as fieldsets partitions and so on

    HtmlBuild = fun({Name, {T, Params}}, Acc) ->
			case lists:keymember(private, 1, Params) of
			    false ->
				[build_html_tag(T, Name, Prefix,
						Params, Default,
						FormType,
						e_conf:primitive_types()) 
				 | Acc];
			    _ ->
				Acc
			end
		end,
    Result = lists:reverse(lists:foldl(HtmlBuild, [], lists:zip(Fields, Types))),

    case wpart:fget("__primary_key") of
	undefined ->
	    Result;
	Val ->
	    {_PKPos, {PKType, PKOpts}} = case wpart_utils:find_pk(Types) of
					     no_pk ->
						 {1, hd(Types)};
					     Else ->
						 Else
					 end,
			     
	    [lists:append(["<input type=\"hidden\" name=\"__primary_key\" value=\"",
			   wpart_utils:term2string(PKType, Val, 
						   proplists:get_value(format, PKOpts, "")), 
			   "\"/>\n"]) | Result]
    end.

-spec(form_type/1 :: (atom()) -> atom()).	     
form_type(table) ->
    table_item;
form_type(list) ->
    list_item;
form_type(paragraph) ->
    paragraph_item;
form_type(_) ->
    div_item.

-spec(load_tpl/0 :: () -> true).	     
load_tpl() ->
    wpart_gen:load_tpl(derived, 
		       filename:join([code:priv_dir(wparts),"html","derived.tpl"])),
    
    wpart_gen:load_tpl(table_item, 
		       filename:join([code:priv_dir(wparts),"html","table_item.tpl"])),

    wpart_gen:load_tpl(paragraph_item, 
		       filename:join([code:priv_dir(wparts),"html","paragraph_item.tpl"])),

    wpart_gen:load_tpl(list_item, 
		       filename:join([code:priv_dir(wparts),"html","list_item.tpl"])),
    
    wpart_gen:load_tpl(div_item, 
		       filename:join([code:priv_dir(wparts),"html","div_item.tpl"])),
    
    wpart_gen:load_tpl(span, 
		       filename:join([code:priv_dir(wparts),"html","span.tpl"])).
