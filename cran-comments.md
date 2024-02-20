This patch release of cocorresp fixes the *two* NOTEs when running R CMD check
under r-devel, as requested by CRAN.

1. The RD markup has been edited to not refer to arguments not in \usage, and
2. Fixes the "undeclared package ‘pls’ in Rd xrefs" NOTE, with pls now listed
  under Suggests in DESCRIPTION.

The package has been checked on multiple platforms and versions of R with zero
NOTES, WARNINGS or ERRORS on any of the systems / R versions tested. This
includes r-devel on Windows (via CRAN's winbuilder system). R versions tested:

* R old-release (Windows, Linux)
* R release (Linux, Windows, MacOS X)
* R devel (Windows, Linux)
