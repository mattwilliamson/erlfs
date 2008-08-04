all:
	erlc -I include -o ebin -v src/*/*.erl
	
clean:
	rm ebin/*.beam
	
