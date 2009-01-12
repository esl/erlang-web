%%% ===================================================================
%%% @author  Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @copyright (C) 2009 Erlang Training & Consulting Ltd.
%%% ===================================================================
{application, eptic_fe,
 [{description, "Frontend Erlang Web application"},
  {vsn, "1.0"},
  {modules, [eptic_fe,e_fe_cache,e_fe_gc,e_fe_proxy,e_fe_mod_yaws,e_fe_mod_inets,e_fe_mod_gen]},
  {registered,[]},
  {env,[]},
  {applications, [kernel, stdlib, sasl, crypto, eptic, wpart]},
  {mod, {eptic_fe,[]}}]}.
