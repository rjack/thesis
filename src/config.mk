CC=gcc
LD=gcc
#CPPOPT=-Wall -DNDEBUG -ansi -pedantic
#CFLAGS=-O3 -fomit-frame-pointer -march=i686 -pipe
CPPOPT=-Wall -ansi -pedantic
CFLAGS=-g -pipe
LDFLAGS=-lc
MAKEDEPEND=gcc -MM $(CPPFLAGS) -o $*.d $<

EXE=ulb
SRC=crono.c dtable_mgr.c if_mgr.c list.c main.c poll_mgr.c socketpair_test.c to_mgr.c util.c
OBJ=$(SRC:.c=.o)
DEP=$(SRC:.c=.P)
