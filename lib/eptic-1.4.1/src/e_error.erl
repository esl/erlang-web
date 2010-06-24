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
%%% @since 1.3
%%% @author Michal Ptaszek <info@erlang-consulting.com>
%%% @doc Module that deals with framework's error representation and translation.
%%% @end
%%%-------------------------------------------------------------------
-module(e_error).

-export([save/2, description/1]).

-define(ERROR_KEY, "__errors").

-spec(save/2 :: (string(), term()) -> true).
save(Field, Error) ->
    eptic:fset(?ERROR_KEY, Field, element(1, Error)).

-spec(description/1 :: (string()) -> string()).
description(Field) ->
    case eptic:fget(?ERROR_KEY, Field) of
	undefined ->
	    [];
	Error ->
	    e_lang:translate(errors, Error)
    end.
