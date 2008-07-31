{application, erlfs_tracker, [{mod, {erlfs_tracker_app, []}},
  	      	      {description, "Erlang distributed file storage system tracker."},
  		      {vsn, "1"},
		      {registered, erlfs_server},
		      {applications, [kernel,
		     		      stdlib,
		     		      mnesia]},
		      {registered, [erlfs_server]}]}.