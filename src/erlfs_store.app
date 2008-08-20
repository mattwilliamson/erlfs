{application, erlfs_store, [{mod, {erlfs_store, []}},
	      {description, "Erlang distributed file storage system storage node application."},
	      {vsn, "alpha"},
	      {registered, [erlfs_store_svr]},
	      {applications, [kernel, stdlib, crypto, sasl]},
	      {modules, [erlfs_store, erlfs_store_sup,
	      		erlfs_store_svr, erlfs_store_worker_sup,
			erlfs_store_worker_fsm, erlfs_store_lib]}]}.