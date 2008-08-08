{application, erlfs_tracker, [{mod, {erlfs_tracker_app, []}},
  	      		     {description, "Erlang distributed 
			     		   file storage system tracker."},
  	      		     {vsn, ".1"},
	      		     {modules, [erlfs_tracker, erlfs.tracker_sup, 
			     	       erlfs_tracker_svr, erlfs_tracker_lib]},
				       {registered, [erlfs_tracker_svr]},
	      			       {applications, [kernel, stdlib, mnesia]}]}.