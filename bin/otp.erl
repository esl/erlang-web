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
%% Ltd. Portions created by Erlang Training & Consulting Ltd are Copyright 2008,
%% Erlang Training & Consulting Ltd. All Rights Reserved.

%%%-----------------------------------------------------------------------------
%%% File    : import.erl
%%% @author Roberto Aloi <info@erlang.consulting.com>
%%% @doc Since the Erlang Web was not allowing the user to add or remove
%%%      OTP applications to/from his Erlang Web project after its creation,
%%%      this script can be used to update the .rel file.
%%%      This solution is extremely "hackish" and it should be handled with
%%%      care, until a better solution is found. You have been warned.
%%% @end
%%%-----------------------------------------------------------------------------

main(["add"]) ->
    App = get_application(),
	add_to_rel_file(App);
main(["remove"]) ->
	App = get_application(),
	remove_from_rel_file(App);
main(["help"]) ->
	help();
main([]) ->
	help().

get_application() ->
	Msg = "Name of your OTP application (no version): ",
    string:strip(io:get_line(Msg), both, 10).

add_to_rel_file(App) ->
	{ok, [{release, Release, ERTS, Apps}]} = file:consult(rel_file()),
	Libs = filelib:wildcard(filename:join([code:lib_dir(), App]) ++ "-*"),
	case Libs of
		[] ->
			io:format("No application found with name: ~p.~n", [App]),
			io:format("Leaving .rel file untouched.~n");
		[Path] ->
			AppFile = filename:join([Path, "ebin", App ++ ".app"]),
			{ok, [{application, _, List}]} = file:consult(AppFile),
			Version = proplists:get_value(vsn, List),
			NewApps = [{list_to_atom(App), Version}|Apps],
			NewContent = {release, Release, ERTS, NewApps},
			write_rel_file(NewContent),
			io:format("Application ~p added.~n", [App]);
	    _ ->
			io:format("Found multiple versions for application ~p.~n", [App]),
			io:format("Leaving .rel file untouched.~n", [])
	end.

remove_from_rel_file(App) ->
	{ok, [{release, Release, ERTS, Apps}]} = file:consult(rel_file()),
	NewApps = proplists:delete(list_to_atom(App), Apps),
	case NewApps of
		Apps ->
			io:format("No application found with name: ~p.~n", [App]),
			io:format("Leaving .rel file untouched.~n");
		_ ->
			NewContent = {release, Release, ERTS, NewApps},
			write_rel_file(NewContent),
			io:format("Application ~p removed.~n", [App])
	end.

rel_file() ->
	% Name and version are hard-coded in the other scripts,
	% so there's no point of using variables here...
	Name = "start",
	filename:join(["releases", "0.1", Name ++ ".rel"]).

write_rel_file(NewContent) ->
	case file:open(rel_file(), [write]) of
		{ok, Fd} ->
			io:format(Fd, "~p.~n", [NewContent]),
			file:close(Fd);
		Else ->
		    Else
	end.

help() ->
	io:format("Add and remove OTP Applications from/to the .rel file:~n~n"),
	io:format("Add an OTP Application to the .rel file:~n"),
	io:format("\tbin/otp.erl add~n"),
	io:format("Remove an OTP Application from the .rel file:~n"),
	io:format("\tbin/otp.erl remove~n").
