FC = gfortran

all: fastestgiftswapintheworld

FFLAGS = -O3
ifeq ($(debug),1)
  FFLAGS = -g -Og -ffpe-trap=zero,overflow -Wall -Wextra -Wconversion -pedantic -std=f2018 -fbacktrace -fcheck=all -fall-intrinsics
endif

fastestgiftswapintheworld: 
	$(FC) $(FFLAGS) -o fastestgiftswapintheworld.exe giftswap.f90

clean:
	rm -f *.o *.mod fastestgiftswapintheworld.exe
