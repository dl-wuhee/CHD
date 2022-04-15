#Python script for automatically generate makefile for fortran source files
#which are all in the same directory
#
#version: 1.7
#fix date: 2013/12/22
#
#All copyrights reserved by Dan Li @ Wuhan University
#Please contact danlee@whu.edu.cn if you have any suggestion
#

#Generated at 2022-04-14 22:04:05

FC=gfortran
 
OPENMPLIBS=-lopenmp
#HDF5HOME=/path/to/hdf5
#HDF5INC=$(HDF5HOME)/include
#HDF5LIB=$(HDF5HOME)/lib
#HDF5LIBS=-lhdf5 -L$(HDF5LIB)
#BLASLIBS=-lblas -L/path/to/blas
#LAPACKLIBS=-llapack -L/path/to/lapack
#MPILIBS=-lmpi -L/path/to/mpi
 
OBJOUTDIR=./../build/obj
#OBJOUTDIR=./intermediate/obj

#MODOUTDIR=$(OBJOUTDIR)
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

FFFLAGS=-std=gnu -Wall -fbounds-check
#FFFLAGS=-std=gnu -Wall -fbounds-check -I$(HDF5INC)
#FFFLAGS=-std=gnu -ffree-form -Wall -g -fbounds-check
LDFLAGS=-L$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS) $(OPENMPLIBS)
#LDFLAGS=$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS) $(BLASLIBS) $(LAPACKLIBS) $(MPILIBS) $(HDF5LIBS)

BOUNDARY_DIR=$(USERDEFAULTMODDIR)
#BOUNDARY_DIR=$(HDF5INC)

SOURCES=config.f90 output.f90 solver.f90 io.f90 log.f90 array.f90 precision.f90\
        main.f90 geometry.f90 mesh.f90 const.f90

OBJS=$(OBJOUTDIR)/config.o $(OBJOUTDIR)/output.o $(OBJOUTDIR)/solver.o $(OBJOUTDIR)/io.o $(OBJOUTDIR)/log.o $(OBJOUTDIR)/array.o $(OBJOUTDIR)/precision.o\
     $(OBJOUTDIR)/main.o $(OBJOUTDIR)/geometry.o $(OBJOUTDIR)/mesh.o $(OBJOUTDIR)/const.o

PROGRAM=./../build/bin/main

$(PROGRAM): $(OBJS)
	 $(FC) -o $(PROGRAM) $^ $(LDFLAGS)

$(OBJOUTDIR)/config.o: config.f90 $(OBJOUTDIR)/io.o $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/output.o: output.f90 $(OBJOUTDIR)/io.o $(OBJOUTDIR)/config.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/solver.o: solver.f90 $(OBJOUTDIR)/geometry.o $(OBJOUTDIR)/config.o $(OBJOUTDIR)/array.o $(OBJOUTDIR)/io.o $(OBJOUTDIR)/mesh.o $(OBJOUTDIR)/precision.o $(OBJOUTDIR)/output.o $(OBJOUTDIR)/log.o $(OBJOUTDIR)/const.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR) -I$(BOUNDARY_DIR) -o $@

$(OBJOUTDIR)/io.o: io.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/log.o: log.f90 $(OBJOUTDIR)/io.o $(OBJOUTDIR)/config.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/array.o: array.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/precision.o: precision.f90 
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/main.o: main.f90 $(OBJOUTDIR)/solver.o $(OBJOUTDIR)/mesh.o $(OBJOUTDIR)/config.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/geometry.o: geometry.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/mesh.o: mesh.f90 $(OBJOUTDIR)/geometry.o $(OBJOUTDIR)/config.o $(OBJOUTDIR)/array.o $(OBJOUTDIR)/precision.o $(OBJOUTDIR)/const.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/const.o: const.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

.PHONY: clean veryclean

clean:
	rm -f $(OBJOUTDIR)/*.o $(MODOUTDIR)/*.mod $(MODOUTDIR)/*.MOD 
veryclean: clean
	rm -f $(PROGRAM)
