{application, rotating_logger, [{mod, {rotating_logger, []}},
	      {description, "Disk event logger which rotates through multiple files when they become 'full' as defined by settings provided."},
	      {vsn, "0.1.0.0"},
	      {registered, [rotating_logger]},
	      {applications, [kernel, stdlib, sasl]},
	      {modules, [rotating_logger]}]}.