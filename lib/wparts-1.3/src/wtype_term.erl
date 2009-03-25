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

%%%=============================================================================
%%% @author Adam Lindberg <info@erlang-consulting.com>
%%% @doc Generic term formatter based on {@module io_lib}·
%%% @end
%%%=============================================================================
-module(wtype_term).
-author('code@erlang-consulting.com').
-copyright('Erlang Training & Consulting Ltd.').
-vsn("$Rev$").

%% Exported formatters.
-export([handle_call/2]).

%%==============================================================================
%% Exported formatters.
%%==============================================================================
%%------------------------------------------------------------------------------
%% @spec handle_call(Format, Term) -> Result
%%	Format = [] | string()
%%	Term = term()
%%	Result = string()
%% 
%% @doc Formats the data with {@link io_lib:format/2}, if `Format' is `[]' it
%% defaults to `~w' formatting.
%% @end
%%------------------------------------------------------------------------------
handle_call([], Term) ->
    io_lib:format("~w", [Term]);
handle_call(Format, Term) ->
    io_lib:format(Format, [Term]).
