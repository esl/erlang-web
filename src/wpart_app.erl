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

start(_, _) ->
    ets_tables_install(),

    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_) ->
    ok.

init([]) ->
    {ok, {{one_for_one, 1, 10}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================

ets_tables_install() ->
    {ok, Types} = file:consult(filename:join([code:priv_dir(wparts),"basic_types.conf"])),
         
    catch ets:delete(basic_types),    
    ets:new(basic_types, [named_table, public]),
    ets:insert(basic_types, Types),
    
    catch ets:delete(templates),
    ets:new(templates, [named_table, public]),
    
    [TypesL] = Types,
    Additional = [form, derived, input],

    lists:foreach(fun(Type) -> 
			  Mod = list_to_atom("wpart_" ++ atom_to_list(Type)),
			  Mod:load_tpl() end, 
		  tuple_to_list(TypesL) ++ Additional).    
