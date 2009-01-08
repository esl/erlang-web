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

-module(wpart_xs).

-include_lib("xmerl/include/xmerl.hrl").
-import(xmerl_xs, [xslapply/2, value_of/1, select/2]).

-export([process_xml/1, template/1]).

process_xml(E) ->
    case application:get_env(wpart, master) of
        {ok, Template} ->
            eptic:fset("wpart_xs_view", E),
            template(eptic:read_file(Template));
        undefined ->
            template(E)
    end.

doctype()->
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n".    

template(#xmlElement{name = 'html'} = E) ->
    ["<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
     doctype(),
     "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n",
     xslapply(fun template/1, E),
     "</html>\n"];

template(#xmlElement{nsinfo = {"wpart", _Operator}} = E) ->
    case wpartlib:eval(E) of
        R when is_list(R) ->
	    xslapply(fun template/1, R);
    	R ->
            built_in_rules(fun template/1, R)
    end;
template(#xmlElement{nsinfo = {"wtpl", "parent"}} = E) ->
    wtpl:build_template(E);
template(#xmlElement{name = Name, attributes = Attr, content = Cont}) ->
    export_tag(Name, Attr, Cont);
template(E) when is_list(E) ->
    xslapply(fun template/1, E);
template(E) ->
    built_in_rules(fun template/1, E).

export_tag(Name, Attributes, []) ->
    [$<, atom_to_list(Name), export_attributes(Attributes), $/,$>];
export_tag(Name, Attributes, Content) ->
    StrName = atom_to_list(Name), 
    [$<, StrName, export_attributes(Attributes),$>, 
     xslapply(fun template/1, Content), 
     $<,$/, StrName, $>].

export_attributes([]) ->
    [];
export_attributes([#xmlAttribute{namespace = {"wpart", N}, value = V}|A]) ->
    export_attributes([#xmlAttribute{name = list_to_atom(N),
				     value = wpartlib:expand_string(V)}|A]);
export_attributes([#xmlAttribute{name = Name, value = Value}|A]) ->
    [$ , atom_to_list(Name), $=, $", Value, $"|export_attributes(A)].

built_in_rules(Fun, E = #xmlElement{})->
    lists:map(Fun, E#xmlElement.content);
built_in_rules(_, E = #xmlText{type=pcdata}) ->
    E#xmlText.value;
built_in_rules(_, E = #xmlText{type=cdata}) -> %% Depricated
    E#xmlText.value;
built_in_rules(_Fun, E = #xmlText{}) ->
    xmerl_lib:export_text(E#xmlText.value);
built_in_rules(_Fun, E = #xmlAttribute{}) ->
    E#xmlAttribute.value;
built_in_rules(_Fun, _E) ->[].

