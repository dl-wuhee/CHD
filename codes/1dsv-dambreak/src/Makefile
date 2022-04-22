#Python script for automatically generate makefile for fortran source files
#which are all in the same directory
#
#version: 1.7
#fix date: 2013/12/22
#
#All copyrights reserved by Dan Li @ Wuhan University
#Please contact danlee@whu.edu.cn if you have any suggestion
#

#Generated at 2022-03-26 15:20:09

FC=gfortran
 
#HDF5HOME=/path/to/hdf5
#HDF5INC=$(HDF5HOME)/include
#HDF5LIB=$(HDF5HOME)/lib
#HDF5LIBS=-lhdf5 -L$(HDF5LIB)
#BLASLIBS=-lblas -L/path/to/blas
#LAPACKLIBS=-llapack -L/path/to/lapack
#MPILIBS=-lmpi -L/path/to/mpi
 
OBJOUTDIR=./../obj
#OBJOUTDIR=./intermediate/obj

MODOUTDIR=$(OBJOUTDIR)
#MODOUTDIR=./intermediate/mod

BINOUTDIR=./../bin

USEDEFAULTHOME=./
USERDEFAULTMODDIR=./
USERDEFAULTLIBDIR=./
USERDEFAULTLIBS=

#USEDEFAULTHOME=./test
#USERDEFAULTMODDIR=$(USEDEFAULTHOME)/include
#USERDEFAULTLIBDIR=$(USEDEFAULTHOME)/lib
#USERDEFAULTLIBS=-la

FFFLAGS=-std=gnu -Wall -fbounds-check -g
#FFFLAGS=-std=gnu -Wall -fbounds-check -I$(HDF5INC)
#FFFLAGS=-std=gnu -ffree-form -Wall -g -fbounds-check
LDFLAGS=-L$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS)
#LDFLAGS=$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS) $(BLASLIBS) $(LAPACKLIBS) $(MPILIBS) $(HDF5LIBS)



SOURCES=precision.f90 main.f90 mesh.f90 solver.f90 const.f90 boudnary.f90 io.f90\
        time.f90 channel.f90 array.f90

OBJS=$(OBJOUTDIR)/precision.o $(OBJOUTDIR)/main.o $(OBJOUTDIR)/mesh.o $(OBJOUTDIR)/solver.o $(OBJOUTDIR)/const.o $(OBJOUTDIR)/boudnary.o $(OBJOUTDIR)/io.o\
     $(OBJOUTDIR)/time.o $(OBJOUTDIR)/channel.o $(OBJOUTDIR)/array.o

PROGRAM=main

$(PROGRAM): $(OBJS)
	 $(FC) -o $(BINOUTDIR)/$(PROGRAM) $^ $(LDFLAGS)

$(OBJOUTDIR)/precision.o: precision.f90 
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/main.o: main.f90 $(OBJOUTDIR)/solver.o $(OBJOUTDIR)/mesh.o $(OBJOUTDIR)/channel.o $(OBJOUTDIR)/time.o $(OBJOUTDIR)/const.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/mesh.o: mesh.f90 $(OBJOUTDIR)/array.o $(OBJOUTDIR)/const.o $(OBJOUTDIR)/channel.o $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/solver.o: solver.f90 $(OBJOUTDIR)/mesh.o $(OBJOUTDIR)/boudnary.o $(OBJOUTDIR)/array.o $(OBJOUTDIR)/channel.o $(OBJOUTDIR)/io.o $(OBJOUTDIR)/time.o $(OBJOUTDIR)/const.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/const.o: const.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/boudnary.o: boudnary.f90 $(OBJOUTDIR)/time.o $(OBJOUTDIR)/const.o $(OBJOUTDIR)/channel.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/io.o: io.f90 $(OBJOUTDIR)/const.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/time.o: time.f90 $(OBJOUTDIR)/const.o $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/channel.o: channel.f90 $(OBJOUTDIR)/const.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/array.o: array.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

.PHONY: clean veryclean

clean:
	rm -f $(OBJOUTDIR)/*.o $(MODOUTDIR)/*.mod $(MODOUTDIR)/*.MOD 
veryclean: clean
	rm -f $(BINOUTDIR)/$(PROGRAM)
