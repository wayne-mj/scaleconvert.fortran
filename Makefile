FC=gfortran
FFLAGS=-O3 -Wall -Wextra
AR=ar
ARARGS=r
MODULES=../fractions/fractionmodule.f90 menumod.f90 trigmod.f90 scaleconverttypes.f90 scaleconvertmodule.f90 
PROG=scaleconvert.f90
SRC=$(MODULES) $(PROG)
OBJ=${SRC:.f90=.o}
BASE=${SRC:.f90=}

all: clean lib scaleconvert

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

scaleconvert: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

lib: $(MODULES)
	$(FC) $(FFLAGS) -c $(MODULES)

dist: clean lib $(MODULES)
	$(AR) $(ARARGS) $(PROG:.f90=.a) $(MODULES:.f90=.o)

clean:
	rm -f *.o *.mod $(BASE) *.a
