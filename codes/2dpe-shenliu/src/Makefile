#Python script for automatically generate makefile for fortran source files
#which are all in the same directory
#
#version: 1.7
#fix date: 2013/12/22
#
#All copyrights reserved by Dan Li @ Wuhan University
#Please contact danlee@whu.edu.cn if you have any suggestion
#

#Generated at 2022-04-17 23:48:59

#HDF5HOME=/path/to/hdf5
#HDF5INC=$(HDF5HOME)/include
#HDF5LIB=$(HDF5HOME)/lib
#HDF5LIBS=-lhdf5 -L$(HDF5LIB)
#BLASLIBS=-lblas -L/path/to/blas
#LAPACKLIBS=-llapack -L/path/to/lapack
#MPILIBS=-lmpi -L/path/to/mpi
 
USEDEFAULTHOME=./
USERDEFAULTMODDIR=./
USERDEFAULTLIBDIR=./
USERDEFAULTLIBS=

#USEDEFAULTHOME=./test
#USERDEFAULTMODDIR=$(USEDEFAULTHOME)/include
#USERDEFAULTLIBDIR=$(USEDEFAULTHOME)/lib
#USERDEFAULTLIBS=-la

FC=gfortran

mode=debug
ifeq ($(mode), debug)
BUILDDIR=./../build/debug
FFFLAGS=-std=gnu -Wall -fbounds-check -g -Ddebug -fopenmp
#FFFLAGS=-std=gnu -Wall -fbounds-check -I$(HDF5INC)
#FFFLAGS=-std=gnu -ffree-form -Wall -g -fbounds-check
LDFLAGS=-fopenmp -L$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS)
#LDFLAGS=$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS) $(BLASLIBS) $(LAPACKLIBS) $(MPILIBS) $(HDF5LIBS)
else
BUILDDIR=./../build/release
FFFLAGS=-std=gnu -Wall -fbounds-check -fopenmp
#FFFLAGS=-std=gnu -Wall -fbounds-check -I$(HDF5INC)
#FFFLAGS=-std=gnu -ffree-form -Wall -g -fbounds-check
LDFLAGS=-fopenmp -O3 -L$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS)
#LDFLAGS=$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS) $(BLASLIBS) $(LAPACKLIBS) $(MPILIBS) $(HDF5LIBS)
endif
OBJOUTDIR=$(BUILDDIR)/obj
MODOUTDIR=$(BUILDDIR)/mod
BINOUTDIR=$(BUILDDIR)/bin
LIBOUTDIR=$(BUILDDIR)/lib

PROGRAM=main_$(mode)

OMP_LIB_DIR=$(USERDEFAULTMODDIR)
#OMP_LIB_DIR=$(HDF5INC)

SOURCES=mesh.f90 config.f90 array.f90 solver.f90 output.f90 geometry.f90 log.f90\
        precision.f90 main.f90 time.f90 const.f90 io.f90

OBJS=$(OBJOUTDIR)/mesh.o $(OBJOUTDIR)/config.o $(OBJOUTDIR)/array.o $(OBJOUTDIR)/solver.o $(OBJOUTDIR)/output.o $(OBJOUTDIR)/geometry.o $(OBJOUTDIR)/log.o\
     $(OBJOUTDIR)/precision.o $(OBJOUTDIR)/main.o $(OBJOUTDIR)/time.o $(OBJOUTDIR)/const.o $(OBJOUTDIR)/io.o


$(PROGRAM): $(OBJS)
	$(FC) -o $(BINOUTDIR)/$(PROGRAM) $^ $(LDFLAGS)

$(OBJOUTDIR)/mesh.o: mesh.f90 $(OBJOUTDIR)/precision.o $(OBJOUTDIR)/const.o $(OBJOUTDIR)/geometry.o $(OBJOUTDIR)/config.o $(OBJOUTDIR)/array.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/config.o: config.f90 $(OBJOUTDIR)/precision.o $(OBJOUTDIR)/io.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/array.o: array.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/solver.o: solver.f90 $(OBJOUTDIR)/log.o $(OBJOUTDIR)/precision.o $(OBJOUTDIR)/mesh.o $(OBJOUTDIR)/const.o $(OBJOUTDIR)/geometry.o $(OBJOUTDIR)/output.o $(OBJOUTDIR)/io.o $(OBJOUTDIR)/config.o $(OBJOUTDIR)/array.o $(OBJOUTDIR)/time.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR) -I$(OMP_LIB_DIR) -o $@

$(OBJOUTDIR)/output.o: output.f90 $(OBJOUTDIR)/config.o $(OBJOUTDIR)/precision.o $(OBJOUTDIR)/io.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/geometry.o: geometry.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/log.o: log.f90 $(OBJOUTDIR)/config.o $(OBJOUTDIR)/precision.o $(OBJOUTDIR)/io.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/precision.o: precision.f90 
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/main.o: main.f90 $(OBJOUTDIR)/config.o $(OBJOUTDIR)/solver.o $(OBJOUTDIR)/mesh.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/time.o: time.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/const.o: const.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/io.o: io.f90 $(OBJOUTDIR)/precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@


$(OBJS): | $(BUILDDIR)

$(BUILDDIR): 
	mkdir -p $(BUILDDIR)/{bin,lib,obj,mod}

.PHONY: clean veryclean

clean:
	rm -f $(OBJOUTDIR)/*.o $(MODOUTDIR)/*.mod $(MODOUTDIR)/*.MOD $(LIBOUTDIR)/*.lib
veryclean: clean
	rm -f $(BINOUTDIR)/$(PROGRAM)
