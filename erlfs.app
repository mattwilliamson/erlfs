{application, erlfs, [
	      	     {mod, {erlfs_application, []}},
  	      	     {description, "Erlang distributed file storage system."},
  		     {vsn, ".01"},
  		     {modules, [
  	    	     	       erlfs_application, 
	    		       erlfs_supervisor,
	    		       erlfs_server,
			       erlfs_server_lib,
			       erlfs_client_lib]}
		]}.