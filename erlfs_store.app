{application, erlfs_store, [{mod, {erlfs_store_app, []}},
  	      	      {description, "Erlang distributed file storage system tracker."},
  		      {vsn, "1"},
		      {registered, erlfs_store},
		      {applications, [kernel,
		     		      stdlib]},
		      {registered, [erlfs_server]}]}.