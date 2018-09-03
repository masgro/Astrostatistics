#******************************** G77/Linux Fortran ************************
#FC     =       gfortran
#EXTRA_OPT =     -mpentium -malign-double -fforce-mem -fforce-addr \
#                -ffast-math -funroll-all-loops
## May want to experiment by adding the extra optimization flags to get
### better runtime. But then again, maybe not.
#FFLAGS  =       -O2 $(EXTRA_OPT) -ffloat-store
#LDFLAGS = 
#time_it         = get_cpu_sun

#******************************** PGI Fortran ************************
#FC      =       pgf77
#FFLAGS  =      -fast
#LDFLAGS =	-fast 
#time_it         = get_cpu_sun

#******************************** Sun Fortran ************************
FC     =       f77
FFLAGS  =      -fast -O
LDFLAGS =	-fast -O
time_it         = get_cpu_sun

#******************************** Lahey-Fujitsu lf95 ************************
#
#FC      =       lf95
#FFLAGS  =       --tpp --nsav -O --nwarn -c
#LDFLAGS =
#time_it         = get_cpu_sun

#****************************************************************************


OBJSB	=	iarray.o \
                isoscatt.o \
                moments.o \
                newphot.o \
                output.o \
                planepar.o \
                ran2.o \

mcgrid:	$(OBJSB)
		$(FC) $(OBJSB) $(LDFLAGS) -o slab

#tarfile:;	tar cvf diskonly.tar *.f *.txt *.in *.par *.com *.dat Makefile oldsrc

clean:;		/bin/rm -f *.o

