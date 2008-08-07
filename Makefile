all:
	
	erlc -I include -pa ebin -o ebin -v src/*/*.erl
	
clean:
	rm ebin/*.beam
	
