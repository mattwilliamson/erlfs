SRCS=src/*.erl src/*/*.erl ebin/*.rel
DOCSRCS=src/*.erl
BIN=ebin

all:
	make bin
	make docs
	
bin:
	cp src/*.rel ebin
	cp src/*.app ebin
	cp src/*/*.app ebin
	erlc -v -I include -pa $(BIN) -o $(BIN) -v $(SRCS)
	
docs: 
	erl -noshell -eval 'edoc:files(filelib:wildcard("$(DOCSRCS)"), [{dir, "doc"}]).' -s init stop
	
docall: 
	erl -noshell -eval 'edoc:files(filelib:wildcard("$(DOCSRCS)"), [{dir, "doc"}, {todo, true}, {private, true}]).' -s init stop
	
clean:
	rm -f $(BIN)/*
	
