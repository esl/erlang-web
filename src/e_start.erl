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
%%% File	: e_start.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Starting module for the Erlang Web framework.
%%% It manages the start sequence and modes in the interactive mode.
%%% API for this module is included in the bin/start_interactive script.
-module(e_start).

-export([start/1]).

%%
%% @spec start(Mode) -> ok | {error, Reason :: term()}
%% Mode :: list(atom())
%% @doc The framework interactive mode starting function.
%% This function will start all required applications (including 
%% three main framework ones: eptic, wpart and wparts).
%% Moreover, if the node is running either in as a frontend
%% or in a both back- and frontend mode,  it will set up desired 
%% server to listen on the port specified in the server 
%% configuration file.
%% @see e_conf:http_port/0
%%
start([Type]) ->
    start([Type, inets]);
start([Type, Server]) ->
    application:start(sasl),
    application:start(ssl),
    application:start(crypto),

    application:start(eptic),
    application:start(wpart),
    application:start(wparts),

    application:set_env(eptic, template_root, "templates"),
    application:set_env(eptic, node_type, Type),

    start_node(Type, Server).

start_node(single_node, Server) ->
    start_web_server(Server),

    e_db:start();
start_node(frontend, Server) ->
    start_web_server(Server),
    
    application:start(eptic_fe);
start_node(backend, _) ->
    e_db:start(),

    supervisor:start_child(eptic, {e_cluster, {e_cluster, start_link, []},
				   permanent, 1000, worker, dynamic});
start_node(single_node_with_cache, Server) ->
    start_web_server(Server),
    
    e_db:start(),
    
    supervisor:start_child(eptic, {e_cluster, {e_cluster, start_link, []},
				   permanent, 1000, worker, dynamic}),
    
    application:start(eptic_fe).

start_web_server(inets) ->
    application:set_env(inets, services, [{httpd, filename:join([e_conf:server_root(), "config", "inets.conf"])},
					  {httpd, filename:join([e_conf:server_root(), "config", "inets_https.conf"])}]),
    
    inets:start();
start_web_server(yaws) ->
    application:set_env(yaws, conf, filename:join([e_conf:server_root(), "config", "yaws.conf"])),

    application:start(yaws).
