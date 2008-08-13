SRCS=src/*.erl src/*/*.erl ebin/*.rel
DOCSRCS=src/*.erl
BIN=ebin

all:
	cp src/*.rel ebin
	cp src/*.app ebin
	cp src/*/*.app ebin
# Compile bytecode
	erlc -I include -pa $(BIN) -o $(BIN) -v $(SRCS)
# Compile documentation
	erl -noshell -eval 'edoc:files(filelib:wildcard("$(DOCSRCS)"), [{dir, "doc"}]).' -s init stop
	
clean:
	rm -f $(BIN)/*
	
