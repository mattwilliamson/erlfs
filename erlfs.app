{application, erlfs, [{mod, {erlfs_application, []}},
  	      	     {description, "Erlang distributed file storage system."},
  		     {vsn, "0.01"},
		     {registered, erlfs_server},
		     {applications, [
		     		    kernel,
		     		    stdlib,
		     		    mnesia]},
  		     {modules, [
  	    	     	       erlfs_application, 
	    		       erlfs_supervisor,
	    		       erlfs_server,
			       erlfs_server_lib,
			       erlfs_client_lib]}]}.