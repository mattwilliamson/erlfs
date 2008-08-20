SRCS=src/*.erl src/*/*.erl
DOCSRCS=src/*.erl
BIN=ebin

all:
	make bin
	make docs
	make boot
	make tar
	
bin:
	cp src/*.app ebin
	cp src/*/*.app ebin
	erlc -v -I include -pa $(BIN) -o $(BIN) -v $(SRCS)
	
boot:
	erl -noshell -pa ./ebin -eval 'systools:make_script("./priv/erlfs").' -s init stop
	
tar:	
	erl -noshell -pa ./ebin -eval 'systools:make_tar("./priv/erlfs").' -s init stop
	
debug:
	erlc -v -I include -pa $(BIN) -o $(BIN) -v $(SRCS) -Deunit
	
docs: 
	erl -noshell -eval 'edoc:files(filelib:wildcard("$(DOCSRCS)"), [{dir, "doc"}]).' -s init stop
	
docall: 
	erl -noshell -eval 'edoc:files(filelib:wildcard("$(DOCSRCS)"), [{dir, "doc"}, {todo, true}, {private, true}]).' -s init stop
	
test:
	escript ./test.erl
	
clean:
	rm -f $(BIN)/*
	rm -f priv/erlfs.tar.gz priv/*.boot priv/*.script
	
