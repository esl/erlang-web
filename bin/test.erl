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
    run_tests("doc/ewts_report");
main(["compile"]) ->
    compile();
main(_) ->
    print_usage().

run_tests(Dir) ->
    start_interactive_mode_node(Dir).
   
start_interactive_mode_node(ReportDir) ->
    Port = open_port({spawn, "bin/start_interactive inets single_node "
		      "-sasl sasl_error_logger false -pa lib/*/test "
		      "-s ewts "
		      "-run ewts start_tests " 
		      ++ ReportDir},
		     [use_stdio, stderr_to_stdout, stream, {line, 1024}]),
    print_output(Port).

compile() ->
    [code:add_path(Dir) || Dir <- filelib:wildcard(filename:join(["lib", "*", "test"]))],
    [code:add_path(Dir) || Dir <- filelib:wildcard(filename:join(["lib", "*", "ebin"]))],
    lists:foreach(fun(App) ->
			  FilesTest = filelib:wildcard(filename:join(["lib", App, "test", "*erl"])),
			  make:files(FilesTest, [{outdir, filename:join(["lib", App, "test"])}, 
						 {i, filename:join(["lib", App, "include"])}])
		  end, get_apps()).

print_output(Port) ->
    receive
	{Port, {data, {eol, "1> EWTSEND"}}} ->
	    port_close(Port);
	{Port, {data, {eol, "1> " ++ Line}}} ->
	    io:format("~s~n", [Line]),
	    print_output(Port);
	{Port, {data, {eol, Line}}} ->
	    io:format("~s~n", [Line]),
	    print_output(Port);
	{Port, {data, _Data}} ->
	    print_output(Port)
    after 10000 ->
	    port_close(Port),
	    ok
    end.

get_apps() ->
    filter(
      element(2, file:list_dir("lib")), []).

filter(["yaws" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["eptic" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["wpart-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["wparts-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["ewts-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["ewgi-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["kernel-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["stdlib-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["mnesia-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["inets-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["mochiweb-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["erlydtl-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["crypto-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["ssl-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["sasl-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter([App | Rest], Acc) ->
    filter(Rest, [App | Acc]);
filter([], Acc) ->
    Acc.

print_usage() ->
    io:format("Usage:~n"
	      "bin/test.erl [run [ReportDir]] - run the tests~n"
	      "bin/test.erl compile - compile the tests - do not run them~n").
