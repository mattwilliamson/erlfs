{application, erlfs_client, [{mod, {erlfs_client_app, []}},
  	      	      {description, "Erlang distributed file storage system tracker."},
  		      {vsn, "1"},
		      {registered, erlfs_client},
		      {applications, [kernel,
		     		      stdlib]},
		      {registered, [erlfs_server]}]}.