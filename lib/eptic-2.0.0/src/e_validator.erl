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
%%% File    : e_validator.erl
%%% Author  : Martin <info@erlang-consulting.com>
%%%           Michal <info@erlang-consulting.com>
%%% Description : Eptic Validator
%%% @hidden
%%%-------------------------------------------------------------------
-module(e_validator).

%% API
-export([email/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
email(Value) ->
    case re:run(Value, "^(([A-Za-z\.]|_|-)+@[A-Za-z]+\.[A-Za-z\.]+[a-z])$",
		[{capture, first, list}]) of
	nomatch ->
	    throw(exception_not_email_address);
	_ ->
	    Value
    end.
    
%%====================================================================
%% Internal functions
%%====================================================================
