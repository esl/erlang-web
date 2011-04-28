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

%%%-----------------------------------------------------------------------------
%%% File    : start.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang.consulting.com>
%%% @doc Compile tool for Erlang Web framework.
%%% It deals with release handling as well.
%%% @end
%%%-----------------------------------------------------------------------------

main([]) ->
    make_basic();
main(["release", Vsn, "yaws"]) ->
    make_release(Vsn, yaws);
main(["release", Vsn, _]) ->
    make_release(Vsn, inets);
main(["release", Vsn]) ->
    make_release(Vsn, inets);
main(["rel", Vsn]) ->
    make_release(Vsn, inets);
main(["release"]) ->
    make_release("0.1", inets);
main(["rel"]) ->
    make_release("0.1", inets);
main(["clean"]) ->
    clean();
main(_) ->
    usage().

make_basic() ->
    {ok, Dir} = file:list_dir("lib/"),
    [code:add_path("lib/" ++ D ++ "/ebin") || D <- Dir],

    compile_e_annotation(),
    Options = get_compiler_options(),

    case make:all(Options) of
	up_to_date ->
	    io:format("Compilation completed!~n");
	error ->
	    io:format("There were errors during the compilation!~n")
    end.

get_compiler_options() ->
    case erlang:system_info(otp_release) > "R14" of
	true -> [{d, r14}];
	false -> []
    end.

compile_e_annotation() ->
    application:load(eptic),
    LibDir = code:lib_dir(eptic),

    compile:file(filename:join([LibDir, "src", "e_annotation.erl"]),
		 [debug_info,
		  {outdir, filename:join([LibDir, "ebin"])}]).

make_release(RelVsn, Server) ->
    make_basic(),
    
    io:format("Starting creating release ~s~n", [RelVsn]),

    check_rel_dir(RelVsn),

    {ok, Dir} = file:list_dir("lib/"),
    ToLoad = lists:map(fun(AppName) ->
			       [Name | _] = string:tokens(AppName, "-"),
			       list_to_atom(Name)
		       end, Dir),
    [application:load(App) || App <- ToLoad],
    
    Loaded = lists:map(fun({Name, _, Vsn}) ->
			       {Name, Vsn}
		       end, application:loaded_applications()),
    
    create_rel_file(RelVsn, Loaded, Server),
    create_boot_file(RelVsn),
    create_sys_config_file(RelVsn, Server),
    update_start_erl_data_file(RelVsn),
    
    io:format("Release ~s compiled~n", [RelVsn]).

check_rel_dir(RelVsn) ->
    Filename = filename:join(["releases", RelVsn]),

    case file:make_dir(Filename) of 
	ok -> confirm_created(Filename);
	{error, eexist} -> inform_exists(Filename);
	{error, Reason} -> handle_error(Reason)
    end.

create_rel_file(RelVsn, Apps, Server) ->
    Name = "start",
    Filename = filename:join(["releases", RelVsn, Name ++ ".rel"]),
    Version = erlang:system_info(version),
    
    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    ReleaseInfo = {release, {Name, RelVsn}, {erts, Version},
			   if
			       Server == inets ->
				   proplists:delete(yaws, Apps);
			       true -> 
				   proplists:delete(inets, Apps)
			   end},
	    
	    io:format(Fd, "~p.~n", [ReleaseInfo]),
	    file:close(Fd),
	    confirm_created(Filename);
	{error, Reason} ->
	    handle_error(Reason)
    end.

create_boot_file(RelVsn) ->
    {ok, _, _} = systools:make_script("start", [{path, ["releases/" ++ RelVsn, "lib/*/ebin"]}, 
						{outdir, "releases/" ++ RelVsn}, silent]),
    confirm_created("releases/" ++ RelVsn ++ "/start.script"),
    confirm_created("releases/" ++ RelVsn ++ "/start.boot"),
    
    file:copy("releases/" ++ RelVsn ++ "/start.rel", "start.rel"),
    {ok, _, _} = systools:make_tar("start",[{path, ["releases/" ++ RelVsn]}, 
					    {outdir, "releases/" ++ RelVsn}, silent]),
    file:delete("start.rel"),

    erl_tar:extract("releases/" ++ RelVsn ++ "/start.tar.gz", [compressed]).

create_sys_config_file(RelVsn, yaws) ->
    YawsConfig = "config/yaws.config",
    case filelib:is_file(YawsConfig) of
	true ->
	    inform_exists(YawsConfig);
	false ->
	    file:copy(code:priv_dir(eptic) ++ "/yaws.conf", YawsConfig),
	    confirm_created(YawsConfig)
    end,
    
    Filename = "releases/" ++ RelVsn ++ "/sys.config",
    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    Content = [{yaws, [{conf, YawsConfig}]}],
	    io:format(Fd, "~p.~n", [Content]),
	    confirm_created(Filename),
	    file:close(Fd);
	{error, Reason} ->
	    handle_error(Reason)
    end;
create_sys_config_file(RelVsn, inets) ->
    MimeTypes = "docroot/conf/mime.types",
    file:copy(code:priv_dir(eptic) ++ "/mime.types", MimeTypes),
    confirm_created(MimeTypes),
   
    InetsConfig = "config/inets.conf",
    case filelib:is_file(InetsConfig) of
	true ->
	    inform_exists(InetsConfig);
	false ->
	    file:copy(code:priv_dir(eptic) ++ "/inets.conf", InetsConfig),
	    confirm_created(InetsConfig)
    end,
    
    Filename = "releases/" ++ RelVsn ++ "/sys.config",
    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    Content = [{inets, [{services, [{httpd, InetsConfig}]}]}],
	    io:format(Fd, "~p.~n", [Content]),
	    confirm_created(Filename),
	    file:close(Fd);
	{error, Reason} ->
	    handle_error(Reason)
    end.

update_start_erl_data_file(RelVsn) ->
    Filename = filename:join("releases", "start_erl.data"),

    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    io:format(Fd, "~s ~s", [RelVsn, erlang:system_info(version)]),
	    file:close(Fd),
	    confirm_created(Filename);
	{error, Reason} ->
	    handle_error(Reason)
    end.

clean() ->
    {ok, Dir} = file:list_dir("lib/"),
    clean(Dir).

clean([]) ->
    io:format("Cleaning up completed!~n");
clean(["yaws" ++ _ | Rest]) ->
    clean(Rest);
clean([".svn" | Rest]) ->
    clean(Rest);
clean([App | Rest]) ->
    io:format("Cleaning up ~s~n", [App]),

    {ok, Files0} = file:list_dir(filename:join(["lib", App, "ebin"])),

    Files = lists:map(fun(Name) ->
			      filename:join(["lib", App, "ebin", Name])
		      end, Files0),

    Beams = lists:filter(fun(Name) ->
				 filename:extension(Name) == ".beam"
			 end, Files),

    Remover = fun(File) ->
		      case file:delete(File) of
			  ok ->
			      confirm_deleted(File);
			  {error, Reason} ->
			      handle_error(Reason)
		      end
	      end,
    lists:foreach(Remover, Beams),
    
    clean(Rest).

usage() ->
    io:format("Usage:~n"
	      "bin/compile.erl - basic compile - just compiles the changed files~n"
	      "bin/compile.erl release - the same as bin/compile.erl release 0.1~n"
	      "bin/compile.erl release Vsn [inets | yaws] - compiles all the sources and create new release - Vsn~n"
	      "bin/compile.erl rel [Vsn] - the same as bin/compile.erl release [Vsn]~n"
	      "By default the selected server is Inets~n").

confirm_created(Name) ->
    io:format("Element created: ~s~n", [Name]).

inform_exists(Name) ->
    io:format("Element exists, skipping: ~s~n", [Name]).

handle_error(Reason) ->
    io:format("An error has occured: ~s~n", [Reason]).

confirm_deleted(Name) ->
    io:format("Element deleted: ~s~n", [Name]).
