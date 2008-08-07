{application, erlfs.store, [{mod, {erlfs.store_app, []}},
	      {description, "Erlang distributed file storage system storage node application."},
	      {vsn, ".1"},
	      {registered, [erlfs.store_svr]},
	      {applications, [kernel, stdlib, crypto]}]}.