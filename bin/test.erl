#!/usr/bin/env escript

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

%%-------------------------------------------------------------------
%%% File    : test.erl
%%% Author  : Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% Description : Escript interface for EWTS application.
%%%
%%% Created : 13 Oct 2009 by michalptaszek <michalptaszek@coltrane.erlangsystems.com>
%%%-------------------------------------------------------------------

main([]) ->
    run_tests("ewts_reports");
main(_) ->
    print_usage().

run_tests() ->
    start_interactive_mode_node().
   
start_interactive_mode_node(ReportDir) ->
    Port = open_port({spawn, "bin/start_interactive -run ewts start_tests" ++ ReportDir},
		     [use_stdio, stderr_to_stdout, stream, {line, 1024}]),
    print_output(Port).

print_output(Port) ->
    receive
	{Port, {data, {eol, "EWTS: " ++ Line}}} ->
	    io:format(Line);
	{Port, {data, _Data}} ->
	    print_output(Port)
    after 1000 ->
	    ok
    end.

print_usage() ->
    io:format("Usage:~n"
	      "bin/test.erl [run] - run the tests~n"
	      "bin/test.erl compile - compile the tests - do not run them~n").
