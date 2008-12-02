CC=gcc
LD=gcc
#CPPOPT=-Wall -DNDEBUG -ansi -pedantic
#CFLAGS=-O3 -fomit-frame-pointer -march=i686 -pipe
CPPOPT=-Wall -ansi -pedantic
CFLAGS=-g -pipe
LDFLAGS=-lc

ULB_EXE=ulb-proto
ULB_OBJ=crono.o dgram.o iface.o util.o list.o

PRX_EXE=proxy

EXE=$(ULB_EXE) $(PRX_EXE)
OBJ=proxy.o main.o $(ULB_OBJ)

DEPCOM=Makefile types.h
