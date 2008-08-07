{application, erlfs.tracker, [{mod, {erlfs.tracker_app, []}},
  	      		     {description, "Erlang distributed 
			     		   file storage system tracker."},
  	      		     {vsn, ".1"},
	      		     {modules, [erlfs.tracker, erlfs.tracker_sup, 
			     	       erlfs.tracker_svr, erlfs.tracker_lib]},
				       {registered, [erlfs.tracker_svr]},
	      			       {applications, [kernel, stdlib, mnesia]}]}.