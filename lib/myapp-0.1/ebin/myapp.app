%% -*- mode: erlang; -*-
{application, myapp,
 [
  {description, ""},
  {vsn, "0.1"}, 
  {modules, [widget_tests, wtype_widget_test]},
  {registered, []},
  {applications, [kernel, stdlib, eptic, wpart, wparts, ewts]},
  {build_dependencies, []},
  {env, []}
 ]}.
