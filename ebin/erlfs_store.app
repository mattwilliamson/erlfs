{application, erlfs_store, [{mod, {erlfs_store_app, []}},
	      {description, "Erlang distributed file storage system storage node application."},
	      {vsn, ".1"},
	      {registered, [erlfs_store_svr]},
	      {applications, [kernel, stdlib]}]}.