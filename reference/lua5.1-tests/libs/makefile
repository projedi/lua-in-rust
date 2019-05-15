CFLAGS= -Wall -O2 -I../../lua-5.1.5/src -ansi -shared

ifeq ($(shell uname), Darwin)
	CFLAGS+= -undefined dynamic_lookup
endif

all: lib1.so lib11.so lib2.so lib21.so -lib2.so

lib1.so: lib1.c
	gcc $(CFLAGS) -o lib1.so lib1.c

lib11.so: lib11.c
	gcc $(CFLAGS) -o lib11.so lib11.c

lib2.so: lib2.c
	gcc $(CFLAGS) -o lib2.so lib2.c

lib21.so: lib21.c
	gcc $(CFLAGS) -o lib21.so lib21.c

-lib2.so: lib2.so
	cp lib2.so ./-lib2.so
