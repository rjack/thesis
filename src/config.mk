CC=gcc
LD=gcc
#CPPOPT=-Wall -DNDEBUG -ansi -pedantic
#CFLAGS=-O3 -fomit-frame-pointer -march=i686 -pipe
CPPOPT=-Wall -ansi -pedantic
CFLAGS=-g -pipe
LDFLAGS=-lc

EXE=ulb
OBJ=main.o crono.o dtable_mgr.o poll_mgr.o to_mgr.o util.o
