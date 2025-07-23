.PHONY: regression
regression: app/*.hs points.raw
	cabal run -- ep47 analyse --baseline --raw points.raw

.PHONY: all
all: images datafiles rawfiles

images: points.png superimpose.png surface.png surface-fixSlope.png surface-fixIntercept.png
datafiles: points.data surface.data surface-fixSlope.data surface-fixIntercept.data
rawfiles: points.raw large.raw

# Raw data

points.raw: app/*.hs Makefile
	cabal run -- ep47 gen-random-data --num-points 20 --intercept 5 --slope 2 --min-distance 0.5 --max-distance 10 --min-deviation -5 --max-deviation 5 --raw points.raw

large.raw: app/*.hs Makefile
	cabal run -- ep47 gen-random-data --num-points 500_000_000 --intercept 5 --slope 2 --min-distance 0.1 --max-distance 100 --min-deviation -50 --max-deviation 50 --raw large.raw

# Gnuplot data

points.data: app/*.hs Makefile
	cabal run -- ep47 gen-random-data --num-points 20 --intercept 5 --slope 2 --min-distance 0.5 --max-distance 10 --min-deviation -5 --max-deviation 5 --gnuplot points.data

surface.data surface-fixSlope.data surface-fixIntercept.data: app/*.hs points.raw
	cabal run -- ep47 gen-regression-surface --raw points.raw

# Images

superimpose.png: points.data
	gnuplot -c gnuplot/superimpose.gnuplot

%.png: %.data
	gnuplot -c gnuplot/$*.gnuplot

# Cleanup

.PHONY: clean
clean:
	rm -f *.data *.raw *.png