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
%%% File    : generate.erl
%%% @author Michal Ptaszek <info@erlang.consulting.com>
%%% @doc Generator script for Erlang Web framework.
%%% @end
%%%-----------------------------------------------------------------------------

-include_lib("kernel/include/file.hrl").

main(["controller" | Rest]) ->
    controller(Rest);
main(["model" | Rest]) ->
    model(Rest);
main(_) ->
    usage().

controller(Args0) ->
    prepare_path(),
    Args = parse_args(Args0, []),

    App = list_to_atom(case proplists:get_value(app, Args) of
			   undefined ->
			       string:strip(io:get_line("Name of the application: "), both, 10);
			   ValA ->
			       ValA
		       end),

    Name = list_to_atom(case proplists:get_value(name, Args) of
			    undefined ->
				string:strip(io:get_line("Name of the controller: "), both, 10);
			    ValN ->
				ValN
			end),
    
    Funs = string:tokens(case proplists:get_value(functions, Args) of
			     undefined ->
				 string:strip(io:get_line("Exported functions (separated with ,): "), both, 10);
			     ValF ->
				 ValF
			 end, [$ , $,]),
    
    SrcFile = filename:join([code:lib_dir(App, src), atom_to_list(Name) ++ ".erl"]),
    case filelib:is_file(SrcFile) of
	true ->
	    io:format("File ~s exists, skipping.~n", [SrcFile]);
	false ->
	    case file:open(SrcFile, [write]) of
		{ok, Fd} ->
		    create_controller(Fd, Name, Funs),
		    add_module_to_app(App, Name),
		    io:format("File ~s created successfully!~n", [SrcFile]);
		{error, Reason} ->
		    io:format("Error during opening ~s for writing, reason: ~p~n", 
			      [SrcFile, Reason])
	    end
    end.

model(Args0) ->
    prepare_path(),
    Args = parse_args(Args0, []),

    App = list_to_atom(case proplists:get_value(app, Args) of
			   undefined ->
			       string:strip(io:get_line("Name of the application: "), both, 10);
			   ValA ->
			       ValA
		       end),

    ModelName = case proplists:get_value(name, Args) of
		    undefined ->
			string:strip(io:get_line("Name of the model: "), both, 10);
		    ValN ->
			ValN
		end,
    
    Hrl = case proplists:get_value(hrl, Args) of
	      undefined ->
		  filename:join(["..", ModelName ++ ".hrl"]);
	      ValH ->
		  ValH
	  end,
    
    SrcFile = filename:join([code:lib_dir(App, src), "wtype_" ++ ModelName ++ ".erl"]),
    case filelib:is_file(SrcFile) of
	true ->
	    io:format("File ~s exists, skipping.~n", [SrcFile]);
	false ->
	    case file:open(SrcFile, [write]) of
		{ok, Fd} ->
		    create_model(Fd, ModelName, Hrl),
		    add_module_to_app(App, list_to_atom("wtype_" ++ ModelName)),
		    io:format("File ~s created successfully!~n", [SrcFile]);
		{error, Reason} ->
		    io:format("Error during opening ~s for writing, reason: ~p~n", 
			      [SrcFile, Reason])
	    end
    end.

-spec(create_controller/3 :: (pid(), atom(), list(string())) -> ok | {error, term()}).	     
create_controller(Fd, Name, Funs) ->
    io:format(Fd, "-module(~p).~n"
	      "-export([",
	      [Name]),
    io:format(Fd, "~s]).~n~n", 
	      [string:join(lists:map(fun(Fun) -> Fun ++ "/1" end, Funs), ", ")]),
    
    controller_function(Fd, Funs),

    file:close(Fd).

-spec(controller_function/2 :: (pid(), list(string())) -> ok).	     
controller_function(Fd, [Fun | Rest]) ->
    io:format(Fd, "~s(_Args) ->~n"
	      "%% put the ~s function body here~n"
	      "    ok.~n~n",
	      [Fun, Fun]),
    controller_function(Fd, Rest);    
controller_function(_, []) ->
    ok.

-spec(create_model/3 :: (pid(), string(), string()) -> ok | {error, term()}).	     
create_model(Fd, Name, Hrl) ->
    VarName = [string:to_upper(hd(Name)) | tl(Name)],

    io:format(Fd, "-module(wtype_~s).~n"
	      "-export([get_record_info/1]).~n"
	      "-export([create/1, read/1, update/1, delete/1]).~n"
	      "-export([prepare_initial/0, prepare_validated/0]).~n~n"
	      "-include(~p).~n~n",
	      [Name, Hrl]),

    io:format(Fd, "get_record_info(~s) -> record_info(fields, ~s);~n"
	      "get_record_info(~s_types) -> #~s_types{}.~n~n",
	      [Name, Name, Name, Name]),

    io:format(Fd, "create(~s) ->~n"
	      "    e_db:write(~s, ~s).~n~n",
	      [VarName, Name, VarName]),
    
    io:format(Fd, "read(all) ->~n"
	      "    e_db:read(~s);~n"
	      "read(Id) ->~n"
	      "    e_db:read(~s, Id).~n~n",
	      [Name, Name]),

    io:format(Fd, "update(~s) ->~n"
	      "    e_db:update(~s, ~s).~n~n",
	      [VarName, Name, VarName]),

    io:format(Fd, "delete(Id) ->~n"
	      "    e_db:delete(~s, Id).~n~n",
	      [Name]),

    io:format(Fd, "prepare_initial() ->~n"
	      "    wpart_db:build_record_structure(~s, #~s{}).~n~n",
	      [Name, Name]),
    
    io:format(Fd, "prepare_validated() ->~n"
	      "    ~s = wpart:fget(\"__not_validated\"),~n"
	      "    wpart_db:build_record_structure(~s, ~s).~n~n",
	      [VarName, Name, VarName]),
    
    file:close(Fd).

-spec(add_module_to_app/2 :: (atom(), atom()) -> ok | {error, term()}).	     
add_module_to_app(AppName, Name) ->
    AppFile = filename:join([code:lib_dir(AppName, ebin)], atom_to_list(AppName) ++ ".app"),
    case file:consult(AppFile) of
	{ok, [{application, AppName, Opts}]} ->
	    Mods0 = proplists:get_value(modules, Opts, []),
	    Mods = case lists:member(Name, Mods0) of
		       true ->
			   Mods0;
		       false ->
			   [Name | Mods0]
		   end,
	    case file:open(AppFile, [write]) of
		{ok, Fd} ->
		    io:format(Fd, "~p.~n",
			      [{application, AppName, 
				[{modules, Mods} | proplists:delete(modules, Opts)]}]),
		    file:close(Fd);
		{error, Reason} ->
		    io:format("Error during opening ~s for writing, reason: ~p~n",
			      [AppFile, Reason])
	    end;
	{error, Reason} ->
	    io:format("Error during opening ~s for reading, reason: ~p~n",
		      [AppFile, Reason])
    end.

-spec(parse_args/2 :: (list(string()), list(tuple())) -> none() | list(tuple())).	     
parse_args(["--" ++ Key, Val | Rest], Acc) ->
    parse_args(Rest, [{list_to_atom(Key), Val} | Acc]);
parse_args([], Acc) ->
    Acc;
parse_args(_, _) ->
    io:format("Badly formatted arguments number, aborting!~n"),
    halt(2).

-spec(prepare_path/0 :: () -> ok).	     
prepare_path() ->
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
		  end, lists:delete(".svn", Libs)).

-spec(usage/0 :: () -> none()).	     
usage() ->
    io:format("Usage: ~s ARGS~n"
	      "where ARGS:~n"
	      "  controller --app APPLICATION_NAME --name NAME --functions FUN1,FUN2,...,FUNN~n"
	      "  model --app APPLICATION_NAME --name NAME [--hrl PATH_TO_HRL]~n",
	      [escript:script_name()]),
    halt(1).
