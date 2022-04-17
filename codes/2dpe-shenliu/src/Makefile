#Python script for automatically generate makefile for fortran source files
#which are all in the same directory
#
#version: 1.7
#fix date: 2013/12/22
#
#All copyrights reserved by Dan Li @ Wuhan University
#Please contact danlee@whu.edu.cn if you have any suggestion
#

#Generated at 2022-04-17 17:24:05

FC=gfortran
 
#HDF5HOME=/path/to/hdf5
#HDF5INC=$(HDF5HOME)/include
#HDF5LIB=$(HDF5HOME)/lib
#HDF5LIBS=-lhdf5 -L$(HDF5LIB)
#BLASLIBS=-lblas -L/path/to/blas
#LAPACKLIBS=-llapack -L/path/to/lapack
#MPILIBS=-lmpi -L/path/to/mpi
 
OBJOUTDIR=./../build/obj
#OBJOUTDIR=./intermediate/obj

MODOUTDIR=./../build/mod
#MODOUTDIR=./intermediate/mod

USEDEFAULTHOME=./
USERDEFAULTMODDIR=./
USERDEFAULTLIBDIR=./
USERDEFAULTLIBS=

#USEDEFAULTHOME=./test
#USERDEFAULTMODDIR=$(USEDEFAULTHOME)/include
#USERDEFAULTLIBDIR=$(USEDEFAULTHOME)/lib
#USERDEFAULTLIBS=-la

FFFLAGS=-std=gnu -Wall -fbounds-check -fopenmp 
#FFFLAGS=-std=gnu -Wall -fbounds-check -I$(HDF5INC)
#FFFLAGS=-std=gnu -ffree-form -Wall -g -fbounds-check
LDFLAGS=-O3 -fopenmp -L$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS)
#LDFLAGS=$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS) $(BLASLIBS) $(LAPACKLIBS) $(MPILIBS) $(HDF5LIBS)

OMP_LIB_DIR=$(USERDEFAULTMODDIR)
#OMP_LIB_DIR=$(HDF5INC)

SOURCES=array.f90 solver.f90 precision.f90 const.f90 time.f90 geometry.f90 mesh.f90\
        log.f90 io.f90 config.f90 output.f90 main.f90

OBJS=$(OBJOUTDIR)/array.o $(OBJOUTDIR)/solver.o $(OBJOUTDIR)/precision.o $(OBJOUTDIR)/const.o $(OBJOUTDIR)/time.o $(OBJOUTDIR)/geometry.o $(OBJOUTDIR)/mesh.o\
     $(OBJOUTDIR)/log.o $(OBJOUTDIR)/io.o $(OBJOUTDIR)/config.o $(OBJOUTDIR)/output.o $(OBJOUTDIR)/main.o

PROGRAM=./../build/bin/main

$(PROGRAM): $(OBJS)
	 $(FC) -o $(PROGRAM) $^ $(LDFLAGS)

$(OBJOUTDIR)/array.o: array.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/solver.o: solver.f90 $(OBJOUTDIR)/config.o $(OBJOUTDIR)/const.o $(OBJOUTDIR)/io.o $(OBJOUTDIR)/geometry.o $(OBJOUTDIR)/output.o $(OBJOUTDIR)/array.o $(OBJOUTDIR)/log.o $(OBJOUTDIR)/precision.o $(OBJOUTDIR)/mesh.o $(OBJOUTDIR)/time.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR) -I$(OMP_LIB_DIR) -o $@

$(OBJOUTDIR)/precision.o: precision.f90 
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/const.o: const.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/time.o: time.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/geometry.o: geometry.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/mesh.o: mesh.f90 $(OBJOUTDIR)/const.o $(OBJOUTDIR)/config.o $(OBJOUTDIR)/geometry.o $(OBJOUTDIR)/array.o $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/log.o: log.f90 $(OBJOUTDIR)/io.o $(OBJOUTDIR)/config.o $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/io.o: io.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/config.o: config.f90 $(OBJOUTDIR)/io.o $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/output.o: output.f90 $(OBJOUTDIR)/io.o $(OBJOUTDIR)/config.o $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/main.o: main.f90 $(OBJOUTDIR)/mesh.o $(OBJOUTDIR)/solver.o $(OBJOUTDIR)/config.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

.PHONY: clean veryclean

clean:
	rm -f $(OBJOUTDIR)/*.o $(MODOUTDIR)/*.mod $(MODOUTDIR)/*.MOD 
veryclean: clean
	rm -f $(PROGRAM)
