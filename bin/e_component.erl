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
%%% File    : e_component.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang.consulting.com>
%%% @doc Script for downloading and installing the e_components for Erlang Web framework.
%%% @end
%%%-----------------------------------------------------------------------------

main(Args) ->
    inets:start(),
    action(Args).

action(["list"]) ->
    Result = make_request("list.py"),

    lists:foreach(fun(R) ->
			  write_result(R)
		  end, lists:sort(string:tokens(Result, "\n")));
action(["search", Keyword]) ->
    Result = make_request("search.py?string=" ++ Keyword),
    
    lists:foreach(fun(R) ->
			  write_result(R)
		  end, lists:sort(string:tokens(Result, "\n")));
action(["details", Name]) ->
    Result = make_request("search.py?string=" ++ Name),
    
    lists:foreach(fun(R) ->
			  write_detailed_result(R)
		  end, lists:sort(string:tokens(Result, "\n")));
action(["install", Name]) ->
    action(["path", "lib/", "install", Name]);
action(["path", Path, "install", Name]) ->
    Result = make_request("download.py?name=" ++ Name),

    erl_tar:extract({binary, list_to_binary(Result)},
		    [{cwd, Path},
		     verbose,
		     compressed,
		     keep_old_files]),

    modify_emakefile(Path, Name),
    
    io:format("~s e_component installed successfully~n", [Name]);
action(_) ->
    io:format("Usage: ~n"
	      "e_component.erl list - lists all of the components stored in the repository~n"

	      "e_component.erl search Keyword - searches in the repository for the component having "
	      "Keyword in its name or description~n"

	      "e_component.erl details Keyword - prints out the detailed information about the given "
	      "component~n"

	      "e_component.erl install Name - downloads and installs the ecomponent into the lib/ "
	      "folder in the current working directory~n"

	      "e_component.erl path Path install Name - downloads and installs the ecomponent into "
	      "the Path directory~n").

write_result(Desc) ->
    [Name, Vsn, Description, _Categories, _Author] = string:tokens(Desc, "\t"),
    io:format("~s ~s~n", [string:left(Name ++ "-" ++ Vsn, 40, $ ), Description]).

write_detailed_result(Desc) ->
    [Name, Vsn, Description, Categories, Author] = string:tokens(Desc, "\t"),
    io:format("Name: ~s Version: ~s\tAuthor: ~s~n", [string:left(Name, 30, $ ), Vsn, Author]),
    io:format("Description:~n~s~n", [Description]),
    io:format("Categories: ~s~n~n", [Categories]).
    
make_request(Type) ->
    case http:request("http://ecomponents.erlang-web.org/cgi-bin/" ++ Type) of
	{ok, {_, _, Result}} ->
	    Result;
	{error, Reason} ->
	    io:format("Error during making the ~p request, reason: ~p~n",
		      [Type, Reason]),
	    []
    end.

modify_emakefile(Path, Name) ->
    case file:consult(filename:join([Path, Name, "Emakefile"])) of
	{ok, Entries} ->
	    case file:open(filename:join([Path, "..", "Emakefile"]), [append]) of
		{ok, Fd} ->
		    lists:map(fun(Entry) ->
				      io:format(Fd, "~p.~n~n", [prepare_entry(filename:join([Path, Name]), Entry)])
			      end, Entries),
		    file:close(Fd);
		{error, Reason} ->
		    error_logger:warning_msg("Error during modifying the root Emakefile, reason: ~p~n",
					     [Reason])
	    end;
	{error, Reason} ->
	    error_logger:warning_msg("Error during reading the e_component's Emakefile, reason: ~p~n",
				     [Reason])
    end.

prepare_entry(Path, {Src, Opts}) ->
    {filename:join([Path, Src]), 
     lists:map(fun(Opt) ->
		       prepare_entry_opt(Path, Opt)
	       end, Opts)}.

prepare_entry_opt(Path, {Type, In}) ->
    {Type, filename:join([Path, In])};
prepare_entry_opt(_, Else) ->
    Else.
