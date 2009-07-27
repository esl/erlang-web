{application, ewgi,
 [{description, "Erlang Webserver Gateway Interface"}
  ,{registered, []}
  ,{vsn, "0.2"}
  ,{applications, [kernel, stdlib]}
  ,{env, []}
  ,{modules, [ewgi_api
              ,ewgi_test
              ,ewgi_inets
              ,ewgi_application
              ,ewgi_mochiweb
              ,ewgi_yaws
             ]}
 ]}.
