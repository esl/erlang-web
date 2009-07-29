{application, wpart, [
	{description, "Wpart"},
	{vsn, "1.3"},
	{modules, [wpart_app,wpart_db,wpart,wpartlib,wpart_gen,
		   wpart_master,wpart_valid,wpart_xs,wtpl,wtype,utf8,
		   utf8_api,wpart_utils,validate_tool,wpart_cache, erlydtl_expander]},
	{applications, [kernel, stdlib, sasl, eptic]},
	{env,[]},
	{registered, []},
        {mod, {wpart_app, []}}]}.
