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
The documentation job detects stale generated files. It uses roxygen2 8.1.0,
matching `Config/roxygen2/version` in DESCRIPTION. When upgrading roxygen2,
update its version in the quality workflow and regenerate documentation together.
Install this version with `pak::pkg_install("roxygen2@8.1.0")` if needed.

Checks run on pull requests and pushes to main/master. Feature-branch pushes
are checked through their PR to avoid duplicate push and PR runs.

Tests use testthat edition 3. Add numerical tests for algorithms and snapshots
for presentation behavior. Image comparisons live in `test-visual.R` and run
only in the dedicated Ubuntu 24.04 / release-R `visual-snapshots` CI job.
All numerical, coordinate, scaling and behavioral plotting assertions still run
throughout the OS/R matrix. Local image comparisons are opt-in:

```r
withr::with_envvar(
  c(COCORRESP_VISUAL_TESTS = "true", NOT_CRAN = "true"),
  testthat::test_local(filter = "visual")
)
```

The existing SVG references passed on Ubuntu release R. Moving them into the
visual test file does not change their contents. Compare failures against the
`visual-snapshot-diffs` artifact from the designated job; local rendering may
differ. Review genuine appearance changes before accepting new snapshots.
R, graphics-device or font upgrades can still require a reference review.
Do not accept platform-only changes from another renderer blindly.

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
