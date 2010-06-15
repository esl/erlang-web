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
%% The Initial Developer of the Original Code is Erlang Solutions Ltd.
%% Portions created by Erlang Solutions Ltd. are Copyright 2008-2010,
%% Erlang Solutions Ltd. All Rights Reserved.

%%%-------------------------------------------------------------------
%%% @copyright (C) 2010, Erlang Solutions Ltd.
%%% @author Magnus Henoch <magnus.henoch@erlang-solutions.com>
%%% @doc Wpart similar to <code>wpart:lookup</code>, except that it
%%% only works for strings, and takes care not to double-encode UTF-8
%%% data.  This will eventually be fixed in <code>wpart:lookup</code>...
%%% @end
-module(wpart_lookup_unbreak).

%% API
-export([handle_call/1]).

-include_lib("xmerl/include/xmerl.hrl").

%%==============================================================================
%% API
%%==============================================================================
handle_call(#xmlElement{} = E) ->
    Key = wpartlib:has_attribute("attribute::key", E),
    case wpart:fget(Key) of
        undefined ->
            "";
        Value ->
            #xmlText{type = cdata,
                     value = wtype_html:htmlize(Value)}
    end.
