{application, erlfs_tracker_app, [{mod, erlfs_tracker_app, []}},
  	      	      {description, "Erlang distributed file storage system tracker."},
  		      {vsn, "1"},
		      {registered, [erlfs_tracker]},
		      {applications, [kernel,
		     		      stdlib,
		     		      mnesia]}]}.