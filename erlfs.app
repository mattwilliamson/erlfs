{application, erlfs, [
	      	     {mod, {erlfs_application, []}},
  	      	     {description, "Erlang distributed file storage."},
  		     {vsn, ".01"},
  		     {modules, [
  	    	     	       erlfs_application, 
	    		       erlfs_supervisor,
	    		       erlfs_server]}
		]}.