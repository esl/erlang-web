{application, ewts,
 [
  {description, "Erlang Web Test Suite"},
  {vsn, "1.0"}, 
  {modules, [e_mod_ewts,
	     ewts_client,
	     ewts_dbg,
	     ewts_sup,
	     ewts_app,
	     ewts]},
  {registered, []},
  {applications, [kernel, stdlib, eptic, wpart, wparts]},
  {build_dependencies, []},
  {mod, {ewts_app, []}},
  {env, []}
 ]}.
