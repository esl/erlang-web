% This is an -*- erlang -*- file.
%%% ===================================================================
%%% @author  Jon Doe <jondoe@erlang-consulting.com>
%%% @copyright (C) 2006 Erlang Training & Consulting Ltd.
%%% @doc
%%% @end 
%%% ===================================================================
{application, wparts,
 [{description, "Wpart Components"},
  {vsn, "1.3"},
  {modules, [wpart_autocomplete,wpart_bool,wpart_choose,wpart_csv,wpart_date,wpart_datetime,wpart_derived,wpart_enum,wpart_form,wpart_include,wpart_input,wpart_integer,wpart_lang,wpart_list,wpart_lookup,wpart_multilist,wpart_password,wpart_string,wpart_text,wpart_time,wpart_upload,wtype_autocomplete,wtype_bool,wtype_csv,wtype_date,wtype_datetime,wtype_enum,wtype_html,wtype_integer,wtype_multilist,wtype_password,wtype_string,wtype_term,wtype_text,wtype_time,wtype_upload,wpart_gen,wpart_float,wtype_float,wpart_paginate,wtype_atom,wpart_atom]},
  {registered,[]},
  {env,[]},
  {applications, [kernel, stdlib, sasl, eptic, wpart]}]}.
