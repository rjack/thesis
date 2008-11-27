CC=gcc
LD=gcc
CPPOPT=-Wall -ansi -pedantic
CFLAGS=-g -pipe
LDFLAGS=-lc

ULB_EXE=ulb-proto
ULB_OBJ=main.o crono.o dgram.o iface.o util.o list.o

PRX_EXE=proxy
PRX_OBJ=proxy.o

EXE=$(ULB_EXE) $(PRX_EXE)
OBJ=$(ULB_OBJ) $(ULB_EXE)

DEPCOM=Makefile types.h
