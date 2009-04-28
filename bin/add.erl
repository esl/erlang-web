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
%%% File    : add.erl
%%% @author Michal Ptaszek <info@erlang.consulting.com>
%%% @doc Script for automatic application adding for Erlang Web framework.
%%% @end
%%%-----------------------------------------------------------------------------

main([]) ->
    {Name, Vsn} = get_description(),

    create_new_application(Name, Vsn),
    update_app_file(Name, Vsn),
    update_makefile(Name, Vsn).

get_description() ->
    Name = string:strip(io:get_line("Name of your application: "), both, 10),
    Vsn0 = string:strip(io:get_line("Version of your application [0.1]: "), both, 10),

    Vsn = case Vsn0 of
	      V when length(V) > 0 ->
		  case catch list_to_float(V) of
		      F when is_float(F) ->
			  V;
		      _ ->
			  case catch list_to_integer(V) of
			      I when is_integer(I) ->
				  V ++ ".0";
			      _ ->
				  "0.1"
			  end
		  end;
	      _ ->
		  "0.1"
	  end,
    
    {Name, Vsn}. 

create_new_application(Name, Vsn) ->
    Filename = filename:join("lib", Name ++ "-" ++ Vsn),

    Creator = fun(X) ->
		      case file:make_dir(X) of 
			  ok -> confirm_created(X);
			  {error, eexist} -> inform_exists(X);
			  {error, Reason} -> handle_error(Reason)
		      end,
		      code:add_patha(X)
	      end,

    Creator(Filename),
    
    AppDirs = ["doc", "ebin", "include", "priv", "src"],
    AppDirsComplete = lists:map(fun(Name0) ->
					filename:join(Filename, Name0)
				end, AppDirs),
    lists:foreach(Creator, AppDirsComplete).

update_app_file(NameS, Vsn) ->
    NameA = list_to_atom(NameS),
    AppFile = filename:join([code:lib_dir(NameA, ebin), NameS ++ ".app"]),
    case file:open(AppFile, [write]) of
	{ok, Fd} ->
	    io:format(Fd, "~p.~n",
		      [{application, NameA, 
			[{description, NameS ++ " description"},
			 {vsn, Vsn},
			 {modules, []},
			 {registered, []},
			 {applications, [kernel, stdlib]}]}]),
	    file:close(Fd),
	    confirm_created(AppFile);
	{error, Reason} ->
	    io:format("Error during opening ~s for writing, reason: ~p~n",
		      [AppFile, Reason])
    end.

update_makefile(Name, Vsn) ->
    Filename = filename:join("lib", Name ++ "-" ++ Vsn),

    case file:open("Emakefile", [append]) of
	{ok, Fd} ->
	    Entry = {list_to_atom(filename:join([Filename, "src", "*"])),
		     [{outdir, filename:join([Filename, "ebin"])},
		      {i, filename:join([Filename, "include"])},
		      debug_info,
		      strict_record_tests,
		      netload]},

	    io:format(Fd, "~n~p.~n", [Entry]),
	    confirm_updated("Emakefile"),
	    
	    file:close(Fd);
	{error, Reason} ->
	    handle_error({"Emakefile", Reason})
    end.

confirm_created(Name) ->
    io:format("Element created: ~s~n", [Name]).

confirm_updated(Name) ->
    io:format("Element updated: ~s~n", [Name]).

inform_exists(Name) ->
    io:format("Element exists, skipping: ~s~n", [Name]).

handle_error(Reason) ->
    io:format("An error has occured: ~s~n", [Reason]).
