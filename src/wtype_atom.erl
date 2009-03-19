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
%%% @author Michal Slaski <info@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wtype_atom).
-behaviour(wtype).

-include_lib("xmerl/include/xmerl.hrl").

-export([handle_call/2,
         validate/1]).

handle_call(_Format, #xmlText{value=Atom}) ->
    #xmlText{value=atom_to_list(Atom)};
handle_call(_Format, Atom) when is_atom(Atom) ->
    atom_to_list(Atom).

validate({_, Atom}) when is_atom(Atom) ->
    {ok, Atom};
validate({_, String}) ->
    case catch list_to_existing_atom(String) of
        {'EXIT',{badarg,_}} ->
            {error, {not_existing, String}};
        Atom ->
            {ok, Atom}
    end.
