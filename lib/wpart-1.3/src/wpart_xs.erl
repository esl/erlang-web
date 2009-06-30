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
%%% @author Martin Carlson <martin@erlang-consulting.com>
%%% @doc Module responsible for template expanding.
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_xs).

-include_lib("xmerl/include/xmerl.hrl").
-import(xmerl_xs, [xslapply/2, value_of/1, select/2]).

-export([process_xml/1, template/1]).

%%
%% @spec process_xml(XMLStructure :: tuple()) -> HTML :: list(string())
%% @doc Processes the XML structure accoring to the built-in rules.
%% Expands the wpart and wtpl tags.
%%
-spec(process_xml/1 :: (tuple()) -> list(string())).
process_xml(E) ->
    template(E).

-spec(doctype/0 :: () -> string()).	     
doctype()->
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n".    

%% @hidden
-spec(template/1 :: (tuple() | list(tuple())) -> list(string()) | string()).
template(#xmlElement{name = 'html'} = E) ->
    ["<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
     doctype(),
     "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n",
     xslapply(fun template/1, E),
     "</html>\n"];

template(#xmlElement{name = 'wpart:cache'} = E) ->
    wpart_cache:process_xml(E);
template(#xmlElement{nsinfo = {"wpart", _Operator}} = E) ->
    case wpartlib:eval(E) of
        R when is_list(R) ->
	    xslapply(fun template/1, R);
    	R ->
            built_in_rules(fun template/1, R)
    end;
template(#xmlElement{nsinfo = {"wtpl", "parent"}} = E) ->
    wtpl:build_template(E);
template(#xmlElement{nsinfo = {"wtpl", "insert"}} = E) ->
    Path = wpartlib:has_attribute("attribute::path", E),
    case catch e_mod_gen:template(Path, [], ?MODULE) of
	{'EXIT', _} ->
	    "";
	{html, HTML} ->
	    HTML
    end;
template(#xmlElement{name = Name, attributes = Attr, content = Cont}) ->
    export_tag(Name, Attr, Cont);
template(E) when is_list(E) ->
    xslapply(fun template/1, E);
template(E) ->
    built_in_rules(fun template/1, E).

-spec(export_tag/3 :: (atom(), list(tuple()), list()) -> list(string()) | string()).	     
export_tag(Name, Attributes, []) ->
    [$<, atom_to_list(Name), export_attributes(Attributes), $/,$>];
export_tag(Name, Attributes, Content) ->
    StrName = atom_to_list(Name), 
    [$<, StrName, export_attributes(Attributes),$>, 
     xslapply(fun template/1, Content), 
     $<,$/, StrName, $>].

-spec(export_attributes/1 :: (list(tuple())) -> list(string()) | string()).
export_attributes([]) ->
    [];
export_attributes([#xmlAttribute{namespace = {"wpart", N}, value = V}|A]) ->
    export_attributes([#xmlAttribute{name = list_to_atom(N),
				     value = wpartlib:expand_string(V)}|A]);
export_attributes([#xmlAttribute{name = Name, value = Value}|A]) ->
    %% Value is a list of Unicode codepoints.  We need to convert that
    %% to UTF-8.
    {ok, EncodedValue} = utf8:to_binary(lists:flatten(Value)),
    %% And we need to use entities for & and ".
    EntitifiedValue = entitify(EncodedValue),
    [$ , atom_to_list(Name), $=, $", EntitifiedValue, $"|export_attributes(A)].

-spec(built_in_rules/2 :: (fun(), tuple()) -> list(string()) | string()).	     
built_in_rules(Fun, E = #xmlElement{})->
    lists:map(Fun, E#xmlElement.content);
built_in_rules(_, E = #xmlText{type=pcdata}) ->
    E#xmlText.value;
built_in_rules(_, E = #xmlText{type=cdata}) -> %% Depricated
    E#xmlText.value;
built_in_rules(_Fun, E = #xmlText{}) ->
    %% Whoa!  Let's take this slowly.
    %%
    %% When xmerl parses a piece of XML, the text of the elements are
    %% not strings, but lists of Unicode codepoints (i.e. lists of
    %% integers that may be > 255).  We need to encode that to UTF-8
    %% before sending it.
    %%
    %% But for the case type=cdata above, we do not encode.  The
    %% wparts that return such things have either built these strings
    %% without access to material from xmerl, or has passed this
    %% material through wpart_xs:template already.  Therefore it is
    %% safe not to encode that text; it would actually be harmful to
    %% do it.
    {ok, B} = utf8:to_binary(lists:flatten(xmerl_lib:export_text(E#xmlText.value))),
    B;
built_in_rules(_Fun, E = #xmlAttribute{}) ->
    E#xmlAttribute.value;
built_in_rules(_Fun, _E) ->
    [].

%% @doc Translate ampersands, single quotes and double quotes to XML
%% entities.
-spec entitify(binary()) -> iolist().
entitify(B) ->
    entitify(B, []).
entitify(<<>>, Acc) ->
    lists:reverse(Acc);
entitify(<<$&:8, Rest/binary>>, Acc) ->
    NewAcc = ["&amp;"|Acc],
    entitify(Rest, NewAcc);
entitify(<<34:8, Rest/binary>>, Acc) ->
    NewAcc = ["&quot;"|Acc],
    entitify(Rest, NewAcc);
entitify(<<39:8, Rest/binary>>, Acc) ->
    NewAcc = ["&apos;"|Acc],
    entitify(Rest, NewAcc);
entitify(<<C:8, Rest/binary>>, Acc) ->
    entitify(Rest, [C|Acc]).
