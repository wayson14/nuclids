FFLAGS=-g -O0 -C  -ffixed-line-length-none
# FFLAGS =   -g -O0  -C -extend_source  -synchronous_exceptions -check underflow -check overflow -warn unused
CFLAGS = -g -O0
FC=gfortran
CC=cc
CPP=g++

all: reformat

reformat: reformat.f
	${FC} ${FFLAGS} -o  reformat reformat.f $(MP_LIB)
