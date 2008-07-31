all:
	erl -noshell -eval 'make:all()' -pa ebin -s erlang halt
	
clean:
	rm ebin/*.beam
	
