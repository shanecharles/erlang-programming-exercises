{application, my_db_gen,
 [{description, "My db server application."},
  {vsn, "1.0"},
  {modules, [my_db_gen, my_db_gen_sup, db, my_db_gen_app]},
  {registered, [my_db_gen, my_db_gen_sup]},
  {applications, [kernel, stdlib]},
  {env, []},
  {mod, {my_db_gen_app, []}}]}.
