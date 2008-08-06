{application, erlfs_client, [{mod, {erlfs_client_app, []}},
  	      {description, "Erlang distributed file storage system client."},
  	      {vsn, ".1"},
	      {registered, [erlfs_client_svr]},
	      {applications, [kernel, stdlib]}]}.