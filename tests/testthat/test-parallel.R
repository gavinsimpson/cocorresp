test_that("permutation designs and serial adapters preserve statistics and RNG", {
  d <- community_fixture()
  fit <- model_fixture("predictive", axes = 2)
  withr::local_seed(100)
  a <- vegan::permutest(fit, permutations = 7, verbose = FALSE)
  seed <- .Random.seed
  set.seed(100)
  b <- vegan::permutest(
    fit,
    permutations = 7,
    verbose = FALSE,
    parallel = lapply
  )
  expect_identical(.Random.seed, seed)
  expect_equal(numeric_result(a), numeric_result(b))
  pm <- rbind(1:16, 16:1)
  expect_s3_class(
    vegan::permutest(fit, permutations = pm, verbose = FALSE),
    "permutest.coca"
  )
  expect_identical(.Random.seed, seed)
  control <- permute::how(nperm = 7, blocks = rep(1:4, each = 4))
  design <- permutation_matrix(control, 16)
  expect_true(all(apply(design, 1, function(p) {
    all(rep(1:4, each = 4)[p] == rep(1:4, each = 4))
  })))
  expect_s3_class(
    vegan::permutest(fit, permutations = control, verbose = FALSE),
    "permutest.coca"
  )
  expect_warning(
    vegan::permutest(fit, n.axes = 9, permutations = 2, verbose = FALSE),
    "reset"
  )
  expect_error(vegan::permutest(model_fixture(), verbose = FALSE), "predcoca")
  for (bad in list(0, NA_real_, -1, 1.5)) {
    expect_error(vegan::permutest(fit, permutations = bad), "positive integer")
  }
  for (bad in list(matrix(1, 2, 16), matrix(1:15, 1), "bad")) {
    expect_error(vegan::permutest(fit, permutations = bad), "row permutations")
  }
  expect_error(make_mapper(0), "positive integer")
  bad_map <- function(X, FUN, ...) rev(lapply(X, FUN, ...))
  expect_error(
    crossval(d$y, d$x, n.axes = 1, verbose = FALSE, parallel = bad_map),
    "input order"
  )
  expect_error(run_task(1L, function(i) stop("failure")), "Task 1: failure")
  expect_error(
    permutation_axis(d$y, d$x[-1, ], NULL, 1, 1, NULL, lapply),
    "rows"
  )
})


test_that("PSOCK workers agree and caller-owned clusters remain usable", {
  skip_on_cran()
  withr::local_seed(42)
  check_backend(2L)
  cl <- parallel::makePSOCKcluster(2L)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  check_backend(cl)
  expect_equal(unlist(parallel::parLapply(cl, 1:2, sqrt)), sqrt(1:2))
  mapper <- make_mapper(cl)
  expect_error(
    mapper$map(1:2, function(i) stop("worker failure")),
    "Task [12].*worker failure"
  )
  mapper$close()
  expect_length(parallel::parLapply(cl, 1:2, identity), 2)
  before <- nrow(showConnections())
  owned <- make_mapper(2L)
  tryCatch(
    owned$map(1:2, function(i) stop("failure")),
    error = function(e) NULL,
    finally = owned$close()
  )
  expect_equal(nrow(showConnections()), before)
})

test_that("mirai clusters implement the same mapping contract", {
  skip_on_cran()
  skip_if_not_installed("mirai", minimum_version = "2.5.0")
  withr::local_seed(42)
  cl <- mirai::make_cluster(2L)
  on.exit(mirai::stop_cluster(cl), add = TRUE)
  check_backend(cl)
  expect_equal(unlist(parallel::parLapply(cl, 1:2, sqrt)), sqrt(1:2))
})

test_that("future and futurize mapping adapters agree", {
  skip_on_cran()
  skip_if_not_installed("future.apply")
  withr::local_seed(42)
  old <- future::plan(future::multisession, workers = 2)
  on.exit(future::plan(old), add = TRUE)
  adapter <- function(X, FUN, ...) {
    future.apply::future_lapply(X, FUN, ..., future.seed = FALSE)
  }
  check_backend(adapter)
  skip_if_not_installed("futurize")
  futuristic <- function(X, FUN, ...) {
    futurize::futurize(lapply(X, FUN, ...), seed = FALSE)
  }
  check_backend(futuristic)
})

test_that("future.mirai can run the future adapter", {
  skip_on_cran()
  skip_if_not_installed("future.mirai")
  withr::local_seed(42)
  old <- future::plan(future.mirai::mirai_multisession, workers = 2)
  on.exit(future::plan(old), add = TRUE)
  adapter <- function(X, FUN, ...) {
    future.apply::future_lapply(X, FUN, ..., future.seed = FALSE)
  }
  check_backend(adapter)
})

test_that("complete blocked enumeration uses the number actually evaluated", {
  d <- community_fixture()
  fit <- coca(d$y[1:4, ], d$x[1:4, ], n.axes = 1)
  control <- permute::how(
    blocks = c(1, 1, 2, 2),
    nperm = 99,
    complete = TRUE
  )
  design <- suppressMessages(permutation_matrix(control, 4))
  expect_identical(nrow(design), 3L)
  expect_true(all(apply(design, 1, function(p) {
    identical(c(1, 1, 2, 2)[p], c(1, 1, 2, 2))
  })))
  result <- suppressMessages(vegan::permutest(
    fit,
    permutations = control,
    verbose = FALSE
  ))
  explicit <- vegan::permutest(fit, permutations = design, verbose = FALSE)
  expect_equal(numeric_result(result), numeric_result(explicit))
  axis <- permutation_axis(
    fit$Ychi$Ychi1,
    fit$Ychi$Ychi2,
    NULL,
    99,
    1,
    design,
    lapply
  )
  expect_length(axis$stati, 4L)
  expect_equal(result$pval, sum(axis$stati >= axis$stati[1]) / 4)
})
