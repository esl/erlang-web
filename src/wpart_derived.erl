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
%%% @author Michal Ptaszek <info@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_derived).
-export([handle_call/1]).
-export([generate_long_name/2, get_description/2, 
	 find/2, surround_with_table/3, load_tpl/0]).

-include_lib("xmerl/include/xmerl.hrl").

%TODO: needs re-writing to handle edit and dynamic feed for some options in one way.
%      now e.g. autocomplete gets feed from dictionary and changes types attributes but edit 
%      gets string with name to inject into wpart. We shoud have in each wpart handling 
%      'default' attribute and this would reduce code. On edit 'engin' work also initial - so 
%      some changes will be nesseccry.
    
%TODO change in tpl ~p to ~s and find into operations in strings
handle_call(E) ->
    case wpartlib:has_attribute("attribute::type", E) of
	false -> [];
	Type -> 
	    Prefix = case wpartlib:has_attribute("attribute::long_name", E) of
			 false -> "";
			 Long -> Long ++ "_"
		     end,
	    Inputs = build_tags(Type, Prefix),

	    Templater = fun(Tag) ->
                                Flatten = "<?xml version=\"1.0\" encoding=\"utf-8\"?>" ++ Tag,
				{XML, _} = xmerl_scan:string(Flatten),
				wpart_xs:template(XML)
			end,
	    Result = string:join(lists:map(Templater, Inputs), "\n"),
	    #xmlText{value=Result,
		     type=cdata}
    end.

check_dict(N) ->
    case wpart:fget(N) of  
        undefined -> true;
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

generate_long_name(Prefix, Name) ->
    Prefix ++ atom_to_list(Name).

get_description(Name, Tuples) ->
    case lists:keysearch(description, 1, Tuples) of
	false when is_atom(Name) -> atom_to_list(Name);
	false -> Name;
	{value, {description, {key, Key}}} ->
	    wpart_lang:get_translation(Key);
	{value, {description, Desc}} -> Desc
    end.

find(Name, List) ->
    case lists:keysearch(Name, 1, List) of
	{value, {_, Val}} ->
	    Val;
	false -> ""
    end.

%% Attributes: [{name_of_field, {choices, "choice1:Text1,choice2:Text2"}}]

handle_types(Fields,Types,Attributes) -> 
handle_types(Fields, Types, Attributes, Fields, []).

handle_types([], [], _, Fields, Types) ->
    lists:zip(Fields, Types);
    
handle_types([Field|T1], [Type|T2], Attributes, Fields, New_Types) ->
    Val = [X || X = {Name, _Att} <- Attributes, Name == Field],
    if 
       Val =/= [] ->
            [{_Name, Att}] = Val,
            {Type_name, List_Att} = Type,
            New_Att = List_Att ++ [Att],
            handle_types(T1, T2, Attributes, Fields, New_Types ++ [{Type_name, New_Att}]);
       true ->
            handle_types(T1,T2, Attributes, Fields, New_Types ++ [Type])
    end.

build_html_tag(Type, Name, Prefix, Params, Default) ->
    case lists:member(Type, e_conf:primitive_types()) of
	true ->
	    Module = list_to_atom("wpart_" ++ atom_to_list(Type)),
	    Module:build_html_tag(Name, Prefix, Params, Default);
	false ->
	    build_html_tag(Type, Name, Prefix, Params)
    end.

build_html_tag(Type, Name, Prefix, Params) ->
    Description = case get_description(Name, Params) of
		      Name ->
			  "";
		      Else ->
			  Else
		  end,
    ListType = atom_to_list(Type),

    [{_, Parts}] = ets:lookup(templates, {wpart, derived}),
    wpart_gen:build_html(Parts, [Prefix ++ ListType, 
				 Description,
				 ListType,
				 Prefix ++ ListType]).

build_tags(Type, Prefix) ->
    Default = case wpart:fget("__edit") of
		  undefined -> [];
		  V -> V
	      end,
    PrimaryKey = case wpart:fget("__primary_key") of
		     undefined -> -1;
		     P -> P
		 end,

    Module = list_to_atom("wtype_" ++ Type),
    Fields = apply(Module, get_record_info,
		   [list_to_atom(Type)]),
    [_ | Types] = tuple_to_list(apply(Module, get_record_info,
				      [list_to_atom(Type ++ "_types")])),
    
    Dynamic =  case wpart:fget("__types") of
		  undefined -> [];
		  V2 -> V2
	      end,

    Info = if 
        Dynamic =/= [] -> 
            handle_types(Fields, Types, Dynamic);
        true -> 
            lists:zip(Fields, Types)
    end,

    HtmlBuild = fun({Name, {T, Params}}, Acc) ->
			Priv = case lists:keysearch(private, 1, Params) of
				   {value, {private, Val}} ->
				       Val;
				   false ->
				       false
			       end,
			
			if
			    Priv == false ->
				[build_html_tag(T, Name, Prefix, 
						Params, Default) 
				 | Acc];
			    true ->
				Acc
			end
		end,
    
    Result = lists:reverse(lists:foldl(HtmlBuild, [], Info)),
    if
	PrimaryKey =/= -1 -> 
	    ["<input type=\"hidden\" name=\"__primary_key\" value=\"" 
		++ integer_to_list(PrimaryKey) ++ "\"/>" | Result];
	true -> Result
    end.

load_tpl() ->
    wpart_gen:load_tpl(derived, 
		       filename:join([code:priv_dir(wparts),"html","derived.tpl"])),
    
    wpart_gen:load_tpl(table_row, 
		       filename:join([code:priv_dir(wparts),"html","table_row.tpl"])).
