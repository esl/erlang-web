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
%%% @author  <info@erlang-consulting.com>
%%% @doc 
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_choose).

%% API
-export([handle_call/1]).
-include_lib("xmerl/include/xmerl.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec handle_call(term()) -> string()
%% @doc WPart component callback, Selects the first child that evals to true
%% @end
%%--------------------------------------------------------------------
handle_call(E) ->
    Paths = wpart:select("wpart:when", E),
    Otherwise = wpart:select("wpart:otherwise", E),
    When = first(fun(When) ->
			 case wpart:has_attribute("attribute::test", When) of
			     false ->
				 case wpart:has_attribute("attribute::wpart:test", When) of
				     false ->
					 false;
				     Test ->
					 F = erl(
					       "fun() -> " ++
					       wpartlib:expand_string(Test) ++
					       " end.", 
					       []),
					 F()
				 end;
			     Test ->
				 F = erl("fun() -> " ++ 
					 wpartlib:expand_string(Test) ++ 
					 " end.", 
					 []),
				 F()
			 end
		 end, Paths),
    if
	When == [], Otherwise == [] ->
	    [];
	When == [] ->
	    Otherwise0 = hd(Otherwise),
	    wpart:eval(Otherwise0#xmlElement.content);
	When /= [] ->
	    wpart:eval(When#xmlElement.content)
    end.

%%====================================================================
%% Internal functions
%%====================================================================
%%-------------------------------------------------------------------
%% @spec erl(Code::string(), Bindings::list()) -> term()
%% @doc Evaluates `Code' with `Bindings' and return the result 
%% @end
%%------------------------------------------------------------------- 
erl(Code1, Bindings) ->
    Code = wpartlib:decode_erl(Code1),

    {ok, Tokens, _} = erl_scan:string(lists:flatten(Code)),
    {ok, Terms} = erl_parse:parse_exprs(Tokens),

    case erl_eval:exprs(Terms, Bindings) of
        {value, Value, _} ->
            Value;
        Error ->
            erlang:error(Error)
    end.

%%-------------------------------------------------------------------
%% @spec first(Fun::fun(), List::list()) -> term() | []
%% @doc Returns the first element satisfying `Pred' or the empty list
%% @end
%%------------------------------------------------------------------- 
first(_, []) ->
    [];
first(Fun, [H|T]) ->
    case Fun(H) of
        true ->
            H;
        false ->
            first(Fun, T)
    end.    
