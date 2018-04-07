all: pascals-triangle

pascals-triangle: pascals-triangle.hs
	ghc --make -O2 pascals-triangle

clean:
	rm pascals-triangle pascals-triangle.hi pascals-triangle.o
