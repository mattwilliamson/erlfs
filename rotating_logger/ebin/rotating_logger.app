{application, rotating_logger, [{mod, {rotating_logger, []}},
	      {description, "Disk event logger which rotates through 
	       multiple files when they become 'full' as defined by 
	       settings provided."},
	      {vsn, "0.2.1.0"},
	      {registered, []},
	      {applications, [kernel, stdlib]},
	      {modules, [rotating_logger]}]}.