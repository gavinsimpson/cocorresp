test_that("environment fitting handles numeric, factor and missing data", {
  fit <- model_fixture()
  env <- data.frame(z = seq_len(16), group = factor(rep(1:2, each = 8)))
  control <- permute::how(nperm = 3, blocks = rep(1:2, each = 8))
  withr::local_seed(42)
  for (e in list(env, env[1], env[2], as.matrix(env[1]), env$z)) {
    out <- vegan::envfit(fit, e, permutations = 0)
    expect_s3_class(out, "envfit")
    expect_null(out$na.action)
  }
  pm <- permute::shuffleSet(16, nset = 3)
  out <- vegan::envfit(fit, env, permutations = pm)
  expect_identical(out$vectors$permutations, 3L)
  expect_identical(out$factors$permutations, 3L)
  expect_s3_class(vegan::envfit(fit, env, permutations = control), "envfit")
  expect_s3_class(vegan::envfit(fit, env, permutations = 3, w = 1), "envfit")
  expect_s3_class(vegan::envfit(fit, env, permutations = 0, w = NULL), "envfit")
  env$z[1] <- NA
  expect_error(vegan::envfit(fit, env, permutations = 0), "missing")
  out <- vegan::envfit(fit, env, permutations = 0, na.rm = TRUE)
  expect_equal(as.integer(out$na.action), 1L)
  expect_s3_class(
    vegan::envfit(fit, env, permutations = control, na.rm = TRUE),
    "envfit"
  )
  expect_s3_class(
    vegan::envfit(
      fit,
      env,
      permutations = 3,
      strata = rep(1:2, each = 8),
      na.rm = TRUE
    ),
    "envfit"
  )
  identity <- matrix(seq_len(16), nrow = 1)
  expect_s3_class(
    vegan::envfit(fit, env, permutations = identity, na.rm = TRUE),
    "envfit"
  )
  expect_error(
    vegan::envfit(fit, env, permutations = pm, na.rm = TRUE),
    "omitted"
  )
  expect_error(
    vegan::envfit(
      fit,
      env,
      permutations = permute::how(within = permute::Within(type = "series")),
      na.rm = TRUE
    ),
    "structured"
  )
  expect_error(vegan::envfit(fit, env[-1, ]), "rows")
  expect_error(vegan::envfit(fit, env, w = c(1, 2)), "w must")
  expect_error(vegan::envfit(fit, env, strata = 1:2), "strata")
  expect_error(vegan::envfit(fit, env, w = 0, na.rm = TRUE), "positive total")
  env$z[-1] <- NA
  expect_error(vegan::envfit(fit, env, na.rm = TRUE), "two complete")
})
