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
%%% File	: erlydtl_expander.erl
%%% @author <info@erlang-consulting.com>
%%% @doc ErlyDTL expander file
%%% @end
%%%-------------------------------------------------------------------
-module(erlydtl_expander).
-export([process_xml/1]).

process_xml(Mod) ->
    case e_dict:get_state() of
        {ok, Dict} ->
            case Mod:render(Dict) of
                {ok, Html} ->
                    [
                        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
                        wpart_xs:doctype(),
                        "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n",
                        Html,
                        "</html>\n"
                    ]
            end;
        _ ->
            exit(no_dict_attached)
    end.
