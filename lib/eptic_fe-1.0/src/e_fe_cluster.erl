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
%%% File    : e_fe_cluster.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Module responsible for managing the frontend nodes of the Erlang Web cluster.
%%% All the functions should be called on the frontend server.
%%% @end
%%%-------------------------------------------------------------------
-module(e_fe_cluster).

-export([write_file/2, write_file_chunk/2]).

%%
%% @spec write_file(Filename :: string(), Content :: binary()) -> ok | {error, Reason :: atom()}
%% @doc Saves the passed <i>Content</i> under the <i>Filename</i> on the frontend node.
%% The previous version of the file is erased.
%%
-spec(write_file/2 :: (string(), binary()) -> ok | {error, atom()}).	   
write_file(Filename, Content) ->
    file:write_file(Filename, Content).

%%
%% @spec write_file_chunk(Filename :: string(), Chunk :: binary()) -> ok
%% @doc Appends the <i>Content</i> to the end of the file specified by <i>Filename</i>.
%% If file does not exist, it is created.
%%
-spec(write_file_chunk/2 :: (string, binary()) -> ok).	     
write_file_chunk(Filename, Content) ->
    case file:open(Filename, [append, raw]) of
	{ok, Fd} ->
	    file:write(Fd, Content),
	    file:close(Fd);
	{error, Reason} ->
	    error_logger:error_msg("~p module, error during opening ~p file, reason: ~p~n", 
				   [?MODULE, Filename, Reason])
    end.
	   
