% This is an -*- erlang -*- file.
%%% ===================================================================
%%% @author  Jon Doe <jondoe@erlang-consulting.com>
%%% @copyright (C) 2006 Erlang Training & Consulting Ltd.
%%% @doc
%%% @end 
%%% ===================================================================
{application, wpart, [
	{description, "Wpart $Rev$"},
	{vsn, "1.3"},
	{modules, [wpart_app,wpart_db,wpart,wpartlib,wpart_master,wpart_valid,wpart_xs,wtpl,wtype,utf8,utf8_api,wpart_utils,validate_tool]},
	{applications, [kernel, stdlib, sasl, eptic]},
	{env,[]},
	{registered, []},
        {mod, {wpart_app, []}}]}.
