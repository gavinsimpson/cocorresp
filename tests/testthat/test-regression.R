test_that("established numerical results remain compatible", {
  withr::local_seed(42)
  ref <- dget(test_path("fixtures", "legacy.R"))
  y <- ref$y
  x <- ref$x
  sym <- coca(y, x, method = "symmetric", n.axes = 2)
  pred <- coca(y, x, n.axes = 2)
  expect_equal(unname(sym$lambda), unname(ref$symmetric), tolerance = 1e-8)
  expect_equal(pred$fitted$Yhat, ref$predicted, tolerance = 1e-8)
  expect_equal(
    crossval(y, x, n.axes = 2, verbose = FALSE)$CVfit,
    ref$cv,
    tolerance = 1e-8
  )
  set.seed(42)
  perm <- vegan::permutest(pred, permutations = 9, n.axes = 2, verbose = FALSE)
  expect_equal(perm[names(ref$permutation)], ref$permutation, tolerance = 1e-8)
})
