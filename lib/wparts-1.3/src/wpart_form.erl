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
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_form).
-export([handle_call/1, load_tpl/0]).

-include_lib("xmerl/include/xmerl.hrl").

handle_call(E) ->
    Value = case wpartlib:has_attribute("attribute::submit_text", E) of
		false ->
		    "";
		Text ->
		    "value='" ++ Text ++ "' "
	    end,

    case wpartlib:has_attribute("attribute::type", E) of 
	false -> [];
	Type -> 
	    case wpartlib:has_attribute("attribute::action", E) of
		false -> 
		    [];
		Action -> 
		    FormType = wpart:has_attribute("attribute::form_type", "div", E),

		    {XML, _} = xmerl_scan:string(build_form(FormType, Type, Action, Value)),
		    Template = wpart_xs:template(XML),

		    #xmlText{value=Template, 
			     type=cdata}
	    end
    end.

build_form(FormType, Name, Action, Value) ->
    Multi = check_for_multipart(Name, e_conf:primitive_types()),
    
    Multipart = if 
		    Multi == true -> "enctype=\"multipart/form-data\"";
		    true -> ""
		end,

    wpart_gen:build_html(wpart_gen:tpl_get(form), [{"action", Action}, 
						   {"multipart", Multipart}, 
						   {"type", Name}, 
						   {"submit", Value},
						   {"form_type", FormType}]).

check_for_multipart(Name, BasicTypes) ->
    Module = list_to_atom("wtype_" ++ Name),
    [_ | Types] = tuple_to_list(apply(Module, get_record_info, 
				      [list_to_atom(Name ++ "_types")])),
    
    Checker = fun(X) ->
		      [Type | _] = tuple_to_list(X),
		      case lists:member(Type, BasicTypes) of
			  true ->
			      if
				  Type == upload -> true;
				  true -> false
			      end;
			  false ->
			      check_for_multipart(atom_to_list(Type), 
						  BasicTypes)
		      end
	      end,

    lists:any(Checker, Types).

load_tpl() ->
    wpart_gen:load_tpl(form, 
		       filename:join([code:priv_dir(wparts), "html", "form.tpl"])).
