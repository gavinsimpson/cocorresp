# Get the version info for later
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

all: check clean

#docs:
#	R -q -e 'library("roxygen2"); roxygenise(".")'

build: #docs
	cd ..;\
	R CMD build cocorresp

check: build
	cd ..;\
	R CMD check cocorresp_$(PKGVERS).tar.gz

check-cran: build
	cd ..;\
	R CMD check --as-cran cocorresp_$(PKGVERS).tar.gz

install: build
	cd ..;\
	R CMD INSTALL cocorresp_$(PKGVERS).tar.gz

move: check
	cp ../cocorresp.Rcheck/cocorresp-Ex.Rout ./tests/Examples/cocorresp-Ex.Rout.save

clean:
	cd ..;\
	rm -r cocorresp.Rcheck/
