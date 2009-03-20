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
%%% File    : wpartlib.erl
%%% @author Martin Carlson <martin@erlang-consulting.com>
%%% @doc Module with helper functions used within wpart application.
%%% @end
%%%-------------------------------------------------------------------
-module(wpartlib_xmerl).

%% API
-export([select/2,
	 has_attribute/2, 
	 attribute/3,
         attribute/2, 
         format/2,
         eval/1,
         eval_file/2,
         expand_string/1,
	 search/2,
	 search/1
	]).

-include_lib("xmerl/include/xmerl.hrl").

%%====================================================================
%% API
%%====================================================================
%% @see wpart:select/2
-spec(select/2 :: (string(), tuple()) -> term()).	     
select(Path, #xmlElement{} = E) ->
    xmerl_xs:select(Path, E).
%% @see wpart:has_attribute/2
-spec(has_attribute/2 :: (string(), tuple()) ->	false | string()).	     
has_attribute(Path, #xmlElement{} = E) ->
    case xmerl_xs:select(Path, E) of
	[#xmlAttribute{value = Value}] ->
	    Value;
	[] ->
	    false
    end.
    
%%
%% @spec attribute(XPath :: string(), XMLElement :: tuple()) -> Value :: string()
%% @doc Retrives the attribute value from the XML element.
%% When the wanted parameter is not present, <i>erlang:error/1</i>
%% is called.
%% 
-spec(attribute/2 :: (string(), tuple()) -> string() | none()).	     
attribute(Path, #xmlElement{} = E) ->
    case xmerl_xs:select(Path, E) of
	[#xmlAttribute{value = Value}] ->
	    Value;
	[] ->
	    erlang:error({Path, not_set})
    end.

%% @see wpart:has_attribute/3
-spec(attribute/3 :: (string(), term(), tuple()) -> term()).	     
attribute(Path, Default, E) ->
    case xmerl_xs:select(Path, E) of
	[#xmlAttribute{value = Value}] ->
	    Value;
	[] ->
	    Default
    end.

%% @see wpart:format/2
-spec(format/2 :: (term(), tuple()) -> term()).	     
format(Value, E) ->
    case has_attribute("attribute::format", E) of
        false ->
            Value;
        Format ->
            wtype:format(Format, Value)
    end.

%% @hidden
%% @FIXME - wrong?
-spec(search/1 :: (term()) -> term()).	     
search(Value) ->
    search(Value, []).

search(Value, []) ->
    Value;
search(Value, [Token|Tokens]) when is_list(Value) ->
    case lists:keysearch(Token, 1, Value) of
	{value, {Token, NewValue}} ->
	    search(NewValue, Tokens);
	false ->
	    undefined
    end;
search(_,_) ->
    undefined.

%% @see wpart:eval_file/2
-spec(eval_file/2 :: (string(), string()) -> list(string()) | string()).	     
eval_file(File, XPath) ->
    eval(xmerl_xs:select(XPath, eptic:read_file(File))).

%%====================================================================
%% Internal functions
%%====================================================================
%% @see wpart:eval/1
-spec(eval/1 :: (tuple() | list(tuple())) -> tuple() | list(tuple())).	     
eval(#xmlElement{nsinfo = {"wpart", Operator}} = E) ->
    Mod = list_to_atom("wpart_" ++ Operator),
    Attr = expand_attributes(E#xmlElement.attributes),
    apply(Mod, handle_call, [E#xmlElement{attributes = Attr}]);
eval(#xmlElement{attributes = Attr, content = []} = E) ->
    E#xmlElement{attributes = expand_attributes(Attr)};
eval(#xmlElement{attributes = Attr} = E) ->
    E#xmlElement{attributes = expand_attributes(Attr),
                 content = eval(E#xmlElement.content)};
eval([H|T]) ->
    [eval(H)|eval(T)]; 
eval(E) ->
    E.

-spec(expand_attributes/1 :: (list()) -> list()).	     
expand_attributes([]) ->
    [];
expand_attributes([#xmlAttribute{namespace = {"wpart", N}, value = V}|T]) ->
    [#xmlAttribute{name = list_to_atom(N), 
		   namespace = [], 
		   nsinfo = [],
		   value = expand_string(V)}|
     expand_attributes(T)];
expand_attributes([A|T]) ->
    [A|expand_attributes(T)].

-spec(expand_string/1 :: (string()) -> string()).	     
expand_string(S) ->
    expand_string(S, [], [[]]).

-spec(expand_string/3 :: (string(), list(), list()) -> string()).	     
expand_string([], _, [Acc0|Acc]) ->
    lists:flatten(lists:reverse([lists:reverse(Acc0)|Acc]));
expand_string([${, H|T], _, [Acc0|Acc]) ->
    expand_string(T, [H], [lists:reverse(Acc0)|Acc]);
expand_string([$}|T], Op0, Acc) ->
    Value = case string:tokens(lists:reverse(Op0), "[]") of
		[Format, Key] ->
		    [Token|Tokens] = string:tokens(Key,":"),
		    wtype:format("[" ++ Format ++ "]", search(wpart:fget(Token), Tokens));
		[Key] ->
		    [Token|Tokens] = string:tokens(Key,":"),
		    case search(wpart:fget(Token), Tokens) of
			[] -> "[]";
			V -> io_lib:format("~p",[V])
		    end
	    end,
    expand_string(T, [], [[], Value |Acc]);
expand_string([H|T], [], [Acc0|Acc]) ->
    expand_string(T, [], [[H|Acc0]|Acc]);
expand_string([H|T], Acc0, Acc) ->
    expand_string(T, [H|Acc0], Acc).
