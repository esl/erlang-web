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

-module(wpart_input).
-export([handle_call/1, load_tpl/0]).

-include_lib("xmerl/include/xmerl.hrl").

-spec(handle_call/1 :: (tuple()) -> tuple()).	     
handle_call(#xmlElement{attributes = Attrs0} = E) ->
    case wpartlib:has_attribute("attribute::type", E) of
	false -> 
	    [];
	Type -> 
	    FormType = case wpartlib:has_attribute("attribute::form_type", E) of
			   false ->
			       undefined;
			   Else ->
			       list_to_atom(Else)
		       end,
	    Attrs = wpart:xml2proplist(Attrs0),
    
	    {XML, _} = xmerl_scan:string(build_tag(FormType, Type, Attrs)),
	    Template = wpart_xs:template(XML),
	    
	    #xmlText{value=Template,
		     type=cdata}
    end.

-spec(build_tag/3 :: (atom(), string(), list()) -> string()).	     
build_tag(FormType, Type, Attrs) ->
    Parts = wpart_gen:tpl_get(wpart, form_type(FormType)),
    wpart_gen:build_html(Parts, [{"id", Type}, 
				 {"form_type", atom_to_list(FormType)},
				 {"type", Type},
				 {"html", wpart:proplist2html(proplists:delete("type", Attrs))}]).

-spec(form_type/1 :: (atom()) -> atom()).	     
form_type(table) ->
    table_form;
form_type(list) ->
    list_form;
form_type(paragraph) ->
    paragraph_form;
form_type(_) ->
    div_form.

-spec(load_tpl/0 :: () -> true).
load_tpl() ->
    wpart_gen:load_tpl(table_form, 
		       filename:join([code:priv_dir(wparts),"html","table_form.tpl"])),

    wpart_gen:load_tpl(paragraph_form, 
		       filename:join([code:priv_dir(wparts),"html","paragraph_form.tpl"])),

    wpart_gen:load_tpl(list_form, 
		       filename:join([code:priv_dir(wparts),"html","list_form.tpl"])),

    wpart_gen:load_tpl(div_form, 
		       filename:join([code:priv_dir(wparts),"html","div_form.tpl"])).
