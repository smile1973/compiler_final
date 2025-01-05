
all: minipython.exe
	./minipython.exe --debug test.py
	gcc -no-pie -g test.s -o a.out
	./a.out

minipython.exe:
	dune build minipython.exe

clean:
	dune clean

.PHONY: all clean minipython.exe



