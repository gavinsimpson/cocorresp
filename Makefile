.PHONY: document test check build pkgdown examples-reference move
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

# Generate the saved output with a plain check. An --as-cran check subsequently
# runs --run-donttest and overwrites cocorresp-Ex.Rout with a different transcript.
examples-reference:
	Rscript -e 'rcmdcheck::rcmdcheck(args = character(), check_dir = ".", error_on = "never")'
	mkdir -p tests/Examples
	cp cocorresp.Rcheck/cocorresp-Ex.Rout tests/Examples/cocorresp-Ex.Rout.save

# Preserve the historical target name.
move: examples-reference
