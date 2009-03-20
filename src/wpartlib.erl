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
%%% File    : wpartlib.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc An interface module to the engine-specific template expander.
%%% @end
%%%-------------------------------------------------------------------

-module(wpartlib).
-export([select/2,
	 has_attribute/2, 
	 attribute/3,
         attribute/2, 
         format/2,
         eval/1,
         eval_file/2,
         expand_string/1,
	 search/2,
	 decode_erl/1
	]).

%% @see wpart:select/2
-spec(select/2 :: (string(), term()) -> term()).
select(Path, XML) ->
    route(select, [Path, XML]).

%% @see wpart:has_attribute/2
-spec(has_attribute/2 :: (string(), term()) -> false | string()).	     
has_attribute(Path, XML) ->
    route(has_attribute, [Path, XML]).

%%
%% @spec attribute(XPath :: string(), XMLElement :: tuple()) -> Value :: string()
%% @doc Retrives the attribute value from the XML element.
%% When the wanted parameter is not present, <i>erlang:error/1</i>
%% is called.
%% 
-spec(attribute/2 :: (string(), term()) -> string() | none()).
attribute(Path, XML) ->
    route(attribute, [Path, XML]).

%% @see wpart:has_attribute/3
-spec(attribute/3 :: (string(), term(), term()) -> term()).	     
attribute(Path, Default, XML) ->
    route(attribute, [Path, Default, XML]).

%% @see wpart:format/2
-spec(format/2 :: (term(), term()) -> term()).	     
format(Value, XML) ->
    route(format, [Value, XML]).

%% @hidden
-spec(search/2 :: (term(), list()) -> term()).	     
search(Value, Tokens) ->
    route(search, [Value, Tokens]).

%% @see wpart:eval/1
-spec(eval/1 :: (tuple() | list(tuple())) -> tuple() | list(tuple())).
eval(Element) ->
    route(eval, [Element]).

-spec(expand_string/1 :: (string()) -> string()).	     
expand_string(String) ->
    route(expand_string, [String]).

-spec(decode_erl/1 :: (string()) -> string()).	     
decode_erl(Code) ->
    lists:foldl(fun({From, To}, Erl) ->
			substitute_erl(Erl, From, To)
		end, Code, [{" lt ", " < "},
			    {" gt ", " > "},
			    {" eq ", " =:= "},
			    {" neq ", " =/= "},
			    {" le ", " =< "},
			    {" ge ", " >= "}]).

%% @see wpart:eval_file/2
-spec(eval_file/2 :: (string(), string()) -> list(string()) | string()).	     
eval_file(File, XPath) ->
    route(eval_file, [File, XPath]).

%%====================================================================
%% Internal functions
%%====================================================================
-spec(route/2 :: (atom(), list()) -> term()).
route(Fun, Attrs) ->
    Mod = case application:get_env(eptic, template_expander) of
	      {ok, wpart_xs} ->
		  wpartlib_xmerl
	  end,

    apply(Mod, Fun, Attrs).

-spec(substitute_erl/3 :: (string(), string(), string()) -> string()).	     
substitute_erl(Erl, From, To) ->
    {ok, NewErl, _} = regexp:gsub(Erl, From, To),
    NewErl.

