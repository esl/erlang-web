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

%%%-------------------------------------------------------------------
%%% File	: e_multipart_common.erl
%%% Author	: Krzysztof Goj <krzysztof.goj@erlang-solutions.com>
%%% Description : Module responsible for server-agnostic things
%%% in multipart form handling
%%% @hidden
%%%-------------------------------------------------------------------

-module(e_multipart_common).

%% API
-export([base_dir/1, delete_tmp_files/1]).

%%====================================================================
%% API
%%====================================================================

%%
%% @doc Returns path to process-specific directory into which files
%% can be stored. This directory will be pruned when process terminates
%%
-spec(base_dir/1 :: (pid()) -> string()).
base_dir(Pid) ->
    % remove angle brackets as they mess up docbuilder
    "<" ++ SafePid = pid_to_list(Pid) -- ">",
    LastDir = "e-multipart-" ++ SafePid,
    filename:join(e_conf:upload_dir(), LastDir).

%%
%% @doc Deletes the directory {@link base_dir/1} with all it's files
%%
-spec(delete_tmp_files/1 :: (pid()) -> ok | {error, atom()}).
delete_tmp_files(Pid) ->
    Dirname = base_dir(Pid),
    lists:foreach(fun file:delete/1,
		  filelib:wildcard(filename:join(Dirname, "*"))),
    file:del_dir(Dirname).
