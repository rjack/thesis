CC=gcc
LD=gcc
#CPPOPT=-std=c99
#CFLAGS=-O3 -fomit-frame-pointer -march=i686 -pipe
CPPOPT=-Wall -Wextra -Wbad-function-cast -std=c99 -pedantic
CFLAGS=-g -pipe
LDFLAGS=-lc
MAKEDEPEND=gcc -MM $(CPPFLAGS) -o $*.d $<

EXE=ulb
SRC=crono.c dtable_mgr.c if_mgr.c ifmon.c list.c main.c poll_mgr.c sim.c to_mgr.c util.c
OBJ=$(SRC:.c=.o)
DEP=$(SRC:.c=.P)
