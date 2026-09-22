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
%%% @author Michal Ptaszek <info@erlang-consulting.com>
%%% @doc Wpart responsible for skipping the content between the tags
%%% (developer comments - they will not appear on the browser side).
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_comment).

-export([handle_call/1]).

-include_lib("xmerl/include/xmerl.hrl").

handle_call(_) ->
    #xmlText{value = ""}.
