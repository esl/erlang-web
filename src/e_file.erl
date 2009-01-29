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
%%% File    : e_file.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Module responsible for manipulating the uploaded files.
%%% @end 
%%%-------------------------------------------------------------------

-module(e_file).
-export([save/2, save/3]).
-export([copy/2, get_relative_name/1]).
-export([get_size/1]).

%%
%% @spec save(Name :: string(), Content :: binary() | string()) -> string()
%% @doc Saves the <i>Content</i> into the specified path inside the upload dir.
%% Returns the path to the saved file (prefixed with upload directory).
%% @see e_conf:upload_dir/0
%%
-spec(save/2 :: (string(), string() | binary()) -> string()).	     
save(Name, Content) when is_list(Content) ->
    save(Name, list_to_binary(Content));
save(Name, Content) when is_binary(Content) ->
    Dir = e_conf:upload_dir(),
    Path = [Dir, "/", Name],
    file:write_file(Path, Content),
    
    Path.

%% 
%% @spec save(Name :: string(), Content :: binary() | string(), Path :: string()) -> string()
%% @doc Saves the <i>Content</i> into the specified file (prefixed with <i>Path</i>).
%% @see save/2
%%
-spec(save/3 :: (string(), string() | binary(), string()) -> string()).	     
save(Name, Content, Path) ->
    save([Path, "/", Name], Content).

%%
%% @spec copy(Source :: string(), Destination :: string()) -> DestinationPath :: string()
%% @doc Copies the file from <i>Source</i> to the <i>Destination</i> in upload directory.
%% Returns the path to the copied file.
%% @see e_conf:upload_dir/0
%%
-spec(copy/2 :: (string(), string()) -> string()).	      
copy(Src, Suffix) ->
    Dir = e_conf:upload_dir(),
    DestDir = [Dir, "/", Suffix],
    Dest = lists:flatten([Dir, "/", Suffix, "/", filename:basename(Src)]),

    file:make_dir(DestDir),    

    file:copy(Src, Dest),
    Dest.

%%
%% @spec get_relative_name(Filename :: string()) -> RelativeFilename :: string()
%% @doc Returns the relative from docroot part of the filename.
%% It is useful when the the database holds the absolute paths to the files
%% (webservers expects the relative from the docroot site).<br/>
%% E.g. when the absolute filename is <i>"/home/erlangweb/docroot/upload/image.png"</i>
%% it will return <i>"/upload/image.png"</i>.
%%
-spec(get_relative_name/1 :: (string()) -> string()).
get_relative_name(Filename) ->	     
    string:sub_string(Filename, length(e_conf:server_root())+8).

%%
%% @spec get_size(Filename :: string()) -> Size :: integer()
%%
-spec(get_size/1 :: (string()) -> integer()).	     
get_size(Filename) ->
    case file:read_file_info(Filename) of
	{ok, #file_info{size = Size}} ->
	    Size;
	{error, Reason} ->
	    error_logger:error_msg("~p module, could not stat ~p, reason ~p~n", 
				   [?MODULE, Filename, Reason]),
	    -1
    end.
