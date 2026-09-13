.PHONY: document test check build pkgdown
document:
	Rscript -e 'devtools::document()'
test:
	Rscript -e 'devtools::test()'
check:
	Rscript -e 'devtools::check()'
build:
	Rscript -e 'devtools::build()'
pkgdown:
	Rscript -e 'pkgdown::build_site(preview = FALSE)'
