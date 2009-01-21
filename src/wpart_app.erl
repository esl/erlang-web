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
%%% File    : wpart_app.erl
%%% Author  : Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% Description : 
%%%
%%%-------------------------------------------------------------------

-module(wpart_app).
-export([start/2, stop/1]).
-export([init/1]).

-behaviour(application).
-behaviour(supervisor).

-define(SUPERVISOR, wpart).

start(_, _) ->
    ets_tables_install(),

    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

stop(_) ->
    ok.

init([]) ->
    {ok, {{one_for_one, 1, 10}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================

ets_tables_install() ->
    {ok, [TypesT]} = file:consult(filename:join([code:priv_dir(wparts),"basic_types.conf"])),
    Types = tuple_to_list(TypesT),
    ets:insert(e_conf, {primitive_types, Types ++ e_conf:primitive_types()}),
    
    catch ets:delete(templates),
    ets:new(templates, [named_table, public]),
    
    Additional = [form, derived, input],
    lists:foreach(fun(Type) -> 
			  Mod = list_to_atom("wpart_" ++ atom_to_list(Type)),
			  Mod:load_tpl() end, 
		  Types ++ Additional).    
