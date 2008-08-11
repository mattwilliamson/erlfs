SRCS=src/*.erl src/*/*.erl
BIN=ebin

all:
# Compile bytecode
	erlc -I include -pa $(BIN) -o $(BIN) -v $(SRCS)
# Compile documentation
	erl -noshell -eval 'edoc:files(filelib:wildcard("src/*.erl"), [{dir, "doc"}]).' -s init stop
	
clean:
	rm -f $(BIN)/*
	
