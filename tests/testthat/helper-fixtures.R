# nolint start: object_usage_linter. Helpers share the testthat environment.
community_fixture <- function() {
  dget(testthat::test_path("fixtures", "legacy.R"))
}

model_fixture <- function(
  method = "symmetric",
  reg.method = "simpls",
  axes = 3
) {
  d <- community_fixture()
  coca(d$y, d$x, method = method, reg.method = reg.method, n.axes = axes)
}

numeric_result <- function(x) x[setdiff(names(x), c("call", "nam.dat"))]

check_backend <- function(backend) {
  d <- community_fixture()
  fit <- model_fixture("predictive", axes = 2)
  serial <- crossval(d$y, d$x, n.axes = 2, verbose = FALSE)
  out <- crossval(d$y, d$x, n.axes = 2, verbose = FALSE, parallel = backend)
  expect_equal(numeric_result(out), numeric_result(serial), tolerance = 1e-8)
  set.seed(23)
  serial <- vegan::permutest(fit, permutations = 5, verbose = FALSE)
  expected_seed <- get(".Random.seed", envir = .GlobalEnv)
  set.seed(23)
  out <- vegan::permutest(
    fit,
    permutations = 5,
    verbose = FALSE,
    parallel = backend
  )
  expect_equal(numeric_result(out), numeric_result(serial), tolerance = 1e-8)
  expect_identical(get(".Random.seed", envir = .GlobalEnv), expected_seed)
}

# nolint end
