CC=gcc
LD=gcc
CPPOPT=-Wall -ansi -pedantic
CFLAGS=-g -pipe
LDFLAGS=-lm -lc

EXE=ulp-proto
OBJ=main.o crono.o

DEPCOM=Makefile types.h
