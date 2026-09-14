test_that("R calculations reproduce archived MATLAB results", {
  ref <- dget(test_path("fixtures", "matlab.R"))
  y <- ref$y
  x <- ref$x

  sym <- coca(y, x, method = "symmetric", n.axes = 2)
  expect_equal(round(unname(sym$lambda), 4), ref$symmetric$lambda)
  expect_equal(
    round(unname(unlist(sym$inertia$total)), 4),
    ref$symmetric$total_inertia
  )
  expect_equal(
    round(unname(unlist(sym$inertia$residual)), 4),
    ref$symmetric$residual_inertia
  )

  # Singular-vector signs are arbitrary, so compare magnitudes with the
  # transition-formula results reported by the MATLAB implementation.
  expect_equal(
    round(abs(unname(sym$scores$species$Y[, 1])), 4),
    ref$transition$species_y_axis1
  )
  expect_equal(
    round(abs(unname(sym$scores$site$Y[, 1])), 4),
    ref$transition$sites_y_axis1
  )

  pred <- coca(y, x, method = "predictive", reg.method = "simpls", n.axes = 2)
  expect_equal(
    round(unname(pred$fitted$Yhat1), 4),
    ref$predictive$fitted
  )
  expect_equal(
    round(crossval(y, x, n.axes = 3, verbose = FALSE)$CVfit, 4),
    ref$predictive$cv_fit
  )

  eig <- coca(y, x, method = "predictive", reg.method = "eigen", n.axes = 3)
  expect_equal(round(unname(eig$lambda), 4), ref$eigen$lambda)

  # The MATLAB transcript does not record the RNG state or permutation
  # matrices, so its stochastic p-values cannot be reproduced. The observed
  # statistics and successive residualisation results are deterministic.
  withr::local_seed(1)
  pred_axes <- coca(
    y,
    x,
    method = "predictive",
    reg.method = "simpls",
    n.axes = 3
  )
  axis_test <- permutest(
    pred_axes,
    permutations = 1,
    n.axes = 3,
    verbose = FALSE
  )
  expect_equal(round(axis_test$permstat, 4), ref$axis_test$statistic)
  expect_equal(round(axis_test$inertia, 4), ref$axis_test$inertia)
  expect_equal(round(axis_test$fitax, 4), ref$axis_test$fit_axis)
  expect_equal(round(axis_test$pcent.fit, 4), ref$axis_test$percentage_fit)
})
