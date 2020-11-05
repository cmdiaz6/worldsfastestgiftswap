FC = gfortran

all: fastestgiftswapintheworld

fastestgiftswapintheworld: 
	$(FC) -O3 -o fastestgiftswapintheworld.exe giftswap.f90

clean:
	rm -f *.o *.mod fastestgiftswapintheworld.exe
