CC=gcc
LD=gcc
CPPOPT=-Wall -Wno-unused -ansi -pedantic
CFLAGS=-g -pipe
LDFLAGS=-lm -lc

EXE=ulp-proto
OBJ=crono.o dgram.o iface.o main.o util.o list.o

DEPCOM=Makefile types.h
