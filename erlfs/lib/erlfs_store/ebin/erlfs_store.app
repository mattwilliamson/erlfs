{application, erlfs_store, [{mod, {erlfs_store, []}},
	      {description, "Erlang distributed file storage system storage node application."},
	      {vsn, "0.1.0.0"},
	      {registered, [erlfs_store_svr]},
	      {applications, [kernel, stdlib, sasl, crypto]},
	      {modules, [erlfs_store, erlfs_store_sup,
	      		erlfs_store_svr, erlfs_store_lib]}]}.
