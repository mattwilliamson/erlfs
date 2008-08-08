SRCS=src/*/*.erl ebin/*.rel
OUT=ebin

all:
	erlc -I include -pa $(OUT) -o $(OUT) -v $(SRCS)
	
clean:
	rm -f $(OUT)/*.beam $(OUT)/*.script $(OUT)/*.boot
	
