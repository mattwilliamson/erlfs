{application, erlfs.client, [{mod, {erlfs.client_app, []}},
  	      {description, "Erlang distributed file storage system client."},
  	      {vsn, ".1"},
	      {registered, [erlfs.client_svr]},
	      {applications, [kernel, stdlib]}]}.