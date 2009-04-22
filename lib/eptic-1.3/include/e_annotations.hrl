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
%%% File    : e_annotations.hrl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Module contatining some pre-defined annotations set.
%%% @end
%%% Created : 22 Apr 2009 by Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%%-------------------------------------------------------------------
-module(e_annotations).

-include_lib("eptic/include/e_annotation.hrl").

-export([invalidate/4, invalidate_if/4]).
-export([invalidate_groups/4, invalidate_groups_if/4]).
-export([backend_call/4]).

?AFTER.
invalidate(Regexps, _Mod, _Fun, FunResult) ->
    case application:get_env(eptic, node_type) of
	{ok, NodeType} when NodeType == backend;
			    NodeType == single_node_with_cache ->
	    e_cluster:invalidate(Regexps);
	_ ->
	    ok
    end,
    FunResult.

?AFTER.
invalidate_if({Regexp, {CMod, CFun}}, _Mod, _Fun, FunResult) ->
    case CMod:CFun(FunResult) of
