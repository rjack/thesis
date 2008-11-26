CC=gcc
LD=gcc
CPPOPT=-Wall -ansi -pedantic
CFLAGS=-g -pipe
LDFLAGS=-lm -lc

EXE=ulb-proto
OBJ=crono.o dgram.o iface.o main.o util.o list.o

DEPCOM=Makefile types.h
