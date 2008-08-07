all:
	erlc -I include -pa ebin -o ebin/erlfs -v src/erlfs/*/*.erl
	erlc -I include -pa ebin -o ebin -v src/*/*.erl
	
clean:
	rm ebin/*.beam
	rm ebin/erlfs*.beam
	
