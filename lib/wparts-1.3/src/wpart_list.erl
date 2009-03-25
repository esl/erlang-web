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
%%% @author Martin Carlson <info@erlang-consulting.com>
%%% @doc 
%%% WPart component for operations on lists
%%% Side effects: eptic:dict
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_list).
-author("support@erlang-consulting.com").
-copyright("Erlang Training & Consulting Ltd.").
-vsn("$Rev").

%% API
-export([handle_call/1]).
-include_lib("xmerl/include/xmerl.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec handle_call(E::#xmlElement{}) -> list() | term()
%% @doc Apply acctions to on lists depending on the select attribute of E
%% @end
%%--------------------------------------------------------------------
handle_call(E) ->
    Select = wpart:has_attribute("attribute::select", E),
    As = wpart:has_attribute("attribute::as", E),
    List = wpart:has_attribute("attribute::list", E),
    Pred = wpart:has_attribute("attribute::pred", E),
    select(Select, As, List, Pred, E).

%%--------------------------------------------------------------------
%% @spec select(Select::string(), 
%%              As::string(), 
%%              List::string(),
%%              Pred::string(), 
%%              E::#xmlElement{}) -> list() | term()
%% @doc Make sure all required values are set
%% @end
%%--------------------------------------------------------------------
select(Select, As, List, _, _) when Select == false; 
                                    As == false; 
                                    List == false ->
    erlang:error({badarg, {Select, As, List}});

%%--------------------------------------------------------------------
%% @spec select(Select::string(), 
%%              As::string(), 
%%              List::string(),
%%              Pred::string(), 
%%              E::#xmlElement{}) -> list() | term()
%% @doc Applies body to to all elements in the list
%% @end
%%--------------------------------------------------------------------
select("map", As, List, _, E) ->
    [FirstKey|KeyList] = string:tokens(List, ":"),
    Data = wpart:fget(FirstKey),
    case wpartlib:search(Data,KeyList) of
        undefined ->
            [];
        EvalList ->
            lists:map(fun(Item) ->
                              wpart:fset(As, Item),
                              wpart:eval(E#xmlElement.content)
                      end, EvalList)
    end;

%%--------------------------------------------------------------------
%% @spec select(Select::string(), 
%%              As::string(),
%%              List::string(),
%%              Pred::string(), 
%%              E::#xmlElement{}) -> list() | term()
%% @doc Returns the head of the list
%% @end
%%--------------------------------------------------------------------
select("head", As, List, _, E) ->
    wpart:fset(As, hd(wpart:fget(List))),
    wpart:eval(E#xmlElement.content);

%%--------------------------------------------------------------------
%% @spec select(Select::string(), 
%%              As::string(),
%%              List::string(),
%%              Pred::string(),
%%              E::#xmlElement{}) -> list() | term()
%% @doc Returns the tail of the list
%% @end
%%--------------------------------------------------------------------
select("tail", As, List, _, E) ->
    wpart:fset(As, tl(wpart:fget(List))),
    wpart:eval(E#xmlElement.content);

%%--------------------------------------------------------------------
%% @spec select(Select::string(), 
%%              As::string(),
%%              List::string(), 
%%              Pred::string(), 
%%              E::#xmlElement{}) -> list() | term()
%% @doc Returns a list of all elements satisfying `Pred'
%% @end
%%--------------------------------------------------------------------
select("filter", As, List, Pred, E) when is_list(Pred) ->
    Fun = erl(Pred, []),
    wpart:fset(As, lists:filter(Fun, wpart:fget(List))),
    wpart:eval(E#xmlElement.content);

%%--------------------------------------------------------------------
%% @spec select(Select::string(), 
%%              As::string(),
%%              List::string(),
%%              Pred::string() 
%%              E::#xmlElement{}) -> list() | term()
%% @doc Returns the first element satisfying `Pred' or the empty list
%% @end
%%--------------------------------------------------------------------
select("find", As, List, Pred, E) when is_list(Pred) ->
    Fun = erl(Pred, []),
    wpart:fset(As, first(Fun, wpart:fget(List))),
    wpart:eval(E#xmlElement.content);

%%--------------------------------------------------------------------
%% @spec select(Select::string(), 
%%              As::string(),
%%              List::string(),
%%              Pred::string(), 
%%              E::#xmlElement{}) -> list() | term()
%% @doc Returns the sorted list sorted by `Pred'
%% @end
%%--------------------------------------------------------------------
select("sort", As, List, Pred, E) when is_list(Pred) ->
    Fun = erl(Pred, []),
    wpart:fset(As, lists:sort(Fun, wpart:fget(List))),
    wpart:eval(E#xmlElement.content).

%%====================================================================
%% Internal functions
%%====================================================================
%%-------------------------------------------------------------------
%% @spec erl(Code::string(), Bindings::list()) -> term()
%% @doc Evaluates `Code' with `Bindings' and return the result 
%% @end
%%------------------------------------------------------------------- 
erl(Code, Bindings) ->
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
