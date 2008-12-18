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
%%% @author  <info@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(wpart_input).
-export([handle_call/1, load_tpl/0]).

-include_lib("xmerl/include/xmerl.hrl").

handle_call(E) ->
    case wpartlib:has_attribute("attribute::type", E) of
	false -> [];
	Type -> {XML, _} = xmerl_scan:string(build_tag(Type)),
		Template = wpart_xs:template(XML),
		#xmlText{value=Template,
			 type=cdata}
    end.

build_tag(Type) ->
    Parts = wpart_gen:tpl_get(wpart, input),
    wpart_gen:build_html(Parts, [Type, Type, Type, Type]).

load_tpl() ->
    wpart_gen:load_tpl(input, 
		       filename:join([code:priv_dir(wparts),"html","input.tpl"])).
