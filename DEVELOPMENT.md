# Development

Use a clean feature branch. Preserve unrelated changes; create an isolated git
worktree when the current checkout is not clean. Do not commit diagnostic output.

Install development dependencies using `pak::local_install_dev_deps()`.
Optional backend tests also use future.apply, futurize, mirai, and future.mirai.

```r
devtools::load_all()
devtools::document()
devtools::test()
devtools::check()
pkgdown::build_site(preview = FALSE)
```

Documentation lives in R comments. Prefer Markdown syntax; retain Rd syntax for
mathematics and specialized constructs. Commit generated Rd and NAMESPACE.
Use `air format R tests` for formatting and `lintr::lint_package()` for checks.
The documentation job detects stale generated files.

Tests use testthat edition 3. Add numerical tests for algorithms and snapshots
for presentation behavior. New snapshots must be reviewed, not accepted blindly.
Run `Sys.setenv(NOT_CRAN = "true")` before tests to exercise worker backends.
Workers load the installed package: install the current checkout before running
parallel tests with `test_local()`. Keep all worker processes on the same version.

Primary CI tests current dependencies on R release, oldrel, and devel. A separate
R 4.1 entry exercises the supported floor using dependency versions compatible
with that R version; documentation is generated on current R only.

```r
cov <- covr::package_coverage()
print(cov)
goodpractice::gp()
```

Aim for at least 95% line coverage and meaningful tests of every retained
function. Treat static-analysis suggestions as review input: S3 methods and
exports are not dead code merely because a static scanner misses their calls.
