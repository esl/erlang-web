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
%%% @doc A start script for the Erlang Web framework.
%%% @end
%%%-----------------------------------------------------------------------------

-include_lib("kernel/include/file.hrl").

main(["yaws"]) ->
    start(yaws);
main(_) ->
    start(inets).

start(Server) ->
    create_start_dirs(),
    RootDir = prepare_paths(),

    Info = get_info(),
    copy_bin_files(Info),

    create_start_scripts(Info),
    create_start_erl_data(Info),
    
    create_basic_config_files(),
    create_welcome_page(),
    
    create_rel_file(Info, Server),

    generate_boot_file(),
    create_sys_config_file(Server),
    
    copy_escripts(RootDir).

confirm_created(Name) ->
    io:format("Element created: ~s~n", [Name]).

inform_exists(Name) ->
    io:format("Element exists, skipping: ~s~n", [Name]).

handle_error(Reason) ->
    io:format("An error has occured: ~s~n", [Reason]).

create_start_dirs() ->
    Creator = fun(X) ->
		      case file:make_dir(X) of 
			  ok -> confirm_created(X);
			  {error, eexist} -> inform_exists(X);
			  {error, Reason} -> handle_error(Reason)
		      end
	      end,
    Dirs = ["config", "docroot", "log", "pipes", "templates", "bin",
	    "lib", "releases", filename:join("releases", "0.1"),
	    filename:join("templates", "cache"),
	    filename:join("docroot", "conf")],
    lists:foreach(Creator, Dirs),
    
    {ok, Apps1} = file:list_dir("lib"),
    Apps = lists:delete(".svn", Apps1),
    LibDirs = lists:map(fun(X) ->
				filename:join("lib", X) 
			end, Apps),
    AppDirs = ["doc", "ebin", "include", "priv", "src"],
    lists:foreach(fun(Lib) ->
			  AppDirsComplete = lists:map(fun(Name) ->
							      filename:join(Lib, Name)
						      end, AppDirs),
			  lists:foreach(Creator, AppDirsComplete)
		  end, LibDirs).

prepare_paths() ->
    ScriptName = escript:script_name(),
    {ok, Dir} = file:get_cwd(),

    Splitted0 = filename:split(ScriptName),
    Splitted = lists:sublist(Splitted0, 1, length(Splitted0)-2),
    RootDir = case length(Splitted) of
		  0 ->
		      Dir;
		  _ ->
		      filename:join([Dir | Splitted])
	      end,

    {ok, Libs} = file:list_dir(filename:join(RootDir, "lib")),

    lists:foreach(fun(Lib) ->
			  Path = filename:join([RootDir, "lib", Lib, "ebin"]),
			  code:add_path(Path)
		  end, lists:delete(".svn", Libs)),
    
    RootDir.
    
get_info() ->
    Dir = code:root_dir(),
    Version = erlang:system_info(version),
    
    {Version, Dir}.

copy_bin_files({Version, Path}) ->
    Prefix = filename:join([Path, "erts-" ++ Version, "bin"]),
    ToCopyList = ["heart", "to_erl", "run_erl"],
    Copier = fun(X) ->
		     Source = filename:join(Prefix, X),
		     Dest = filename:join("bin", X),
		     case file:copy(Source, Dest) of
			 {ok, _} -> confirm_created(Dest);
			 {error, Reason} -> handle_error(Reason)
		     end,
		     file:write_file_info(Dest, #file_info{mode=8#00744})
	     end,
    lists:foreach(Copier, ToCopyList).

create_script(FileName, Content) ->
    case file:open(FileName, [write]) of
	{ok, Fd} ->
	    io:format(Fd, Content, []),
	    file:close(Fd),
	    file:write_file_info(FileName, 
				 #file_info{mode=8#00744}),
	    confirm_created(FileName);
	{error, Reason} -> handle_error(Reason)
    end.
			
create_start_scripts({_, Path}) ->
    BinStart = "#!/bin/sh\n\n"

	"ROOTDIR=`pwd`\n"
	"export TERM=xterm\n"
	"export SHELL=/bin/bash\n"
	"export HEART_COMMAND=\"$ROOTDIR/bin/start\"\n\n"

	"echo \"Starting Erlang Web\"\n"
	"RELDIR=$ROOTDIR/releases\n"
	"rm -f $ROOTDIR/erlang.log.?\n"
	"START_ERL_DATA=${1:-$RELDIR/start_erl.data}\n"
	"$ROOTDIR/bin/run_erl -daemon pipes/ $ROOTDIR/log \"exec $ROOTDIR/bin/start_erl " 
	"$ROOTDIR $RELDIR $START_ERL_DATA\"\n",
    
    BinStartName = filename:join("bin", "start"),
    create_script(BinStartName, BinStart),

    BinStop = "#!/bin/sh\n\n"

	"ROOTDIR=`pwd`\n"
	"PID=$(ps ax | grep -E .*beam.*$ROOTDIR | grep -v grep | awk '{print $1}')\n\n"

	"if [ $PID ]; then\n"
	"\techo  \"Stopping Erlang Web\"\n"
	"\tHEART_PID=$(ps ax | grep heart | grep -v beam | grep $PID | awk '{print $1}')\n"
	"\tif [ $HEART_PID ]; then\n"
	"\t\tkill $HEART_PID\n"
	"\telse\n"
	"\t\tkill $PID\n"
	"\tfi\n"
	"else\n"
	"\techo \"Erlang Web is not running\"\n"
	"fi\n",
    
    BinStopName = filename:join("bin", "stop"),
    create_script(BinStopName, BinStop),

    BinConnect = "#!/bin/sh\n\n"
	"bin/to_erl pipes/\n",
    
    BinConnectName = filename:join("bin", "connect"),
    create_script(BinConnectName, BinConnect),

    BinStartInteractive = "#!/bin/sh\n\n"

"if [ $# -eq 0 ]
then
    SERVER=inets
    NODE_TYPE=single_node
elif [ $# -eq 1 ]
then
    case $1 in
	yaws)
	    SERVER=yaws
	    ;;
	*)
	    SERVER=inets
    esac
    NODE_TYPE=single_node
else
    case $1 in
	yaws)
	    SERVER=yaws
	    ;;
	*)
	    SERVER=inets
    esac
    case $2 in
	frontend)
	    NODE_TYPE=frontend
	    ;;
	backend)
	    NODE_TYPE=backend
	    ;;
	single_node_with_cache)
	    NODE_TYPE=single_node_with_cache
	    ;;
	*)
	    NODE_TYPE=single_node
    esac
fi

erl -pa lib/*/ebin -s e_start start $NODE_TYPE $SERVER",
    
    BinStartInteractiveName = filename:join("bin", "start_interactive"),
    create_script(BinStartInteractiveName, BinStartInteractive),

    FileContent2 = "#!/bin/sh\n\n"

	"ROOTDIR=$1\n"
	"RELDIR=$2\n"
	"DataFile=$3\n\n"

	"shift\nshift\nshift\n\n"

	"ERTS_VSN=`awk '{print $2}' $DataFile`\n"
	"VSN=`awk '{print $1}' $DataFile`\n\n"

	"BINDIR=" ++ Path ++ "/erts-$ERTS_VSN/bin\n"
	"EMU=beam\n"
	"PROGNAME=`echo $0 | sed 's/.*\\///'`\n"
	"HOSTNAME=test\n\n"

	"export EMU\n"
	"export ROOTDIR\n"
	"export BINDIR\n"
	"export PROGNAME\n"
	"export RELDIR\n\n"

	"exec $BINDIR/erlexec -boot $RELDIR/$VSN/start -config $RELDIR/$VSN/sys "
	"-heart -env HEART_BEAT_TIMEOUT 30 -pa patches +K true -sname $HOSTNAME -smp auto +P 262140 ${1+\"$@\"}\n",
    
    FileName2 = filename:join("bin", "start_erl"),
    create_script(FileName2, FileContent2).

create_start_erl_data({Version, _}) ->
    Filename = filename:join("releases", "start_erl.data"),

    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    io:format(Fd, "0.1 ~s", [Version]),
	    file:close(Fd),
	    confirm_created(Filename);
	{error, Reason} ->
	    handle_error(Reason)
    end.

create_basic_config_files() ->
    conf_dispatcher(),
    conf_errors(),
    conf_project().

conf_dispatcher() ->
    Filename = filename:join(["config", "dispatch.conf"]),

    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    io:format(Fd, "{static, \"[/]*\", \"welcome.html\"}.", []),
	    file:close(Fd),
	    confirm_created(Filename);
	{error, Reason} ->
	    handle_error(Reason)
    end.

conf_errors() ->
    Filename = filename:join(["config", "errors.conf"]),

    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    io:format(Fd, "{error, 501, \"templates/501.html\"}.\n"
		      "{error, 404, \"templates/404.html\"}.\n", []),
	    file:close(Fd),
	    confirm_created(Filename),
	    
	    ErrorPage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
		"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" "
		"\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
		"<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
		"<head>\n"
		"<title>~p Error</title>\n"
		"</head>\n"
		"<body>\n"
		"<center>\n"
		"<h1>~p Error</h1>\n"
		"</center>\n"
		"</body>\n"
		"</html>",

	    Filename404 = filename:join("templates", "404.html"),
	    case file:open(Filename404, [write]) of
		{ok, Fd404} ->
		    io:format(Fd404, ErrorPage, [404, 404]),
		    file:close(Fd404),
		    confirm_created(Filename404);
		{error, Reason1} ->
		    handle_error(Reason1)
	    end,
	    
	    Filename501 = filename:join(["templates", "501.html"]),
	    case file:open(Filename501, [write]) of
		{ok, Fd501} ->
		    io:format(Fd501, ErrorPage, [501, 501]),
		    file:close(Fd501),
		    confirm_created(Filename501);
		{error, Reason2} ->
		    handle_error(Reason2)
	    end;
	{error, Reason} ->
	    handle_error(Reason)
    end.

conf_project() ->
    Filename = filename:join(["config", "project.conf"]),

    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    io:format(Fd, "{http_port, 8080}.~n", []),
	    file:close(Fd),
	    confirm_created(Filename);
	{error, Reason} ->
	    handle_error(Reason)
    end.

create_welcome_page() ->
    Filename = filename:join(["templates", "welcome.html"]),

    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    Content = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
		"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n"
		"\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
		"<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\n"
    
		"<head>\n"
		"<title>Erlang Web</title>\n"
		"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n"
		"</head>\n"
		"<body>\n"
		"<h1>Welcome to the Erlang Web!</h1>\n"
		"</body>\n"
		"</html>\n",
	    io:format(Fd, Content, []),
	    file:close(Fd),
	    confirm_created(Filename);
	{error, Reason} ->
	    handle_error(Reason)
    end.

create_rel_file({Version, _}, Server) ->
    Name = "start",
    Filename = filename:join(["releases", "0.1", Name ++ ".rel"]),
    
    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    Apps = get_apps_for_release(Server),
	    ReleaseInfo = {release, {Name, "0.1"}, {erts, Version},
			   Apps},
	    
	    io:format(Fd, "~p.~n", [ReleaseInfo]),
	    file:close(Fd),
	    confirm_created(Filename);
	{error, Reason} ->
	    handle_error(Reason)
    end.

get_apps_for_release(Server) ->
    {ok, Dir} = file:list_dir("lib/"),
    [code:add_path("lib/" ++ D ++ "/ebin") || D <- Dir],

    ToLoad = [xmerl, sasl, crypto, eptic, wpart, wparts, mnesia, ssl, Server],
    [application:load(App) || App <- ToLoad],

    lists:map(fun({Name, _, Vsn}) ->
		      {Name, Vsn}
	      end, application:loaded_applications()).

generate_boot_file() ->
    systools:make_script("start", [{path, ["releases/0.1", "lib/*/ebin"]}, {outdir, "releases/0.1/"}, silent]),
    confirm_created("releases/0.1/start.script"),
    confirm_created("releases/0.1/start.boot"),
    
    file:copy("releases/0.1/start.rel", "start.rel"),
    systools:make_tar("start",[{path, ["releases/0.1"]}, {outdir, "releases/0.1/"}, {dirs, [include]}, silent]),
    file:delete("start.rel"),
    
    erl_tar:extract("releases/0.1/start.tar.gz", [keep_old_files, compressed]).

create_sys_config_file(yaws) ->
    YawsConfig = "config/yaws.conf",
    file:copy(code:priv_dir(yaws) ++ "/yaws.conf", YawsConfig),
    confirm_created(YawsConfig),
    
    Filename = "releases/0.1/sys.config",
    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    Content = [{yaws, [{conf, YawsConfig}]}],
	    io:format(Fd, "~p.~n", [Content]),
	    confirm_created(Filename),
	    file:close(Fd);
	{error, Reason} ->
	    handle_error(Reason)
    end;
create_sys_config_file(inets) ->
    MimeTypes = "docroot/conf/mime.types",
    file:copy(code:priv_dir(eptic) ++ "/mime.types", MimeTypes),
    confirm_created(MimeTypes),

    InetsConfig = "config/inets.conf",
    file:copy(code:priv_dir(eptic) ++ "/inets.conf", InetsConfig),
    confirm_created(InetsConfig),

    Filename = "releases/0.1/sys.config",
    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    Content = [{inets, [{services, [{httpd, InetsConfig}]}]}],
	    io:format(Fd, "~p.~n", [Content]),
	    confirm_created(Filename),
	    file:close(Fd);
	{error, Reason} ->
	    handle_error(Reason)
    end.

copy_escripts(RootDir) ->
    case file:get_cwd() of
	{ok, RootDir} ->
	    ok;
	{ok, _} ->
	    Files = [filename:join(["bin", "compile.erl"]),
		     filename:join(["bin", "add.erl"])],
	    
	    file:copy(filename:join(RootDir, "Emakefile"), "Emakefile"),
	    confirm_created("Emakefile"),
	    
	    lists:foreach(fun(File) ->
				  file:copy(filename:join([RootDir, File]), File),
				  file:write_file_info(File, 
						       #file_info{mode=8#00744}),
				  confirm_created(File)
			  end, Files)
    end.
