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
%%% @version $Rev$
%%% @author <info@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(wpart_include).

-export([handle_call/1]).
-include_lib("xmerl/include/xmerl.hrl").

handle_call(E) ->
    File = wpart:has_attribute("attribute::file", E),
    As = wpart:has_attribute("attribute::as", E),
    Format = wpart:has_attribute("attribute::format", E),

    {ok, Bin} = file:read_file(sanitise_filename(File)),
    Tpl = binary_to_list(Bin),
    if
	As == false, Format == false ->
	    #xmlText{value = Tpl};
	As == false ->
	    wpartlib:format(#xmlText{value = Tpl}, E);
	Format == false ->
	    wpart:fset(As, Tpl);
	true ->
	    wpart:fset(As, wpartlib:format(Tpl, E))
    end.

sanitise_filename([$/|T]) ->
    sanitise_filename0(T);
sanitise_filename(T) ->
    sanitise_filename0(T).

sanitise_filename0([$.,$.,$/|T]) ->
    sanitise_filename0(T);
sanitise_filename0([$.,$.|T]) ->
    sanitise_filename0(T);
sanitise_filename0([H|T]) ->
    [H|sanitise_filename0(T)];
sanitise_filename0([]) ->
    [].
