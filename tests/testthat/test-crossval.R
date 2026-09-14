test_that("crossval() works", {
  data(beetles, plants, package = "cocorresp")
  beetles <- log(beetles + 1) # log transform the bettle data
  expect_message(
    bp.loo <- crossval(beetles, plants, n.axes = 2, verbose = FALSE),
    "Removed"
  )
  expect_s3_class(bp.loo, "crossval")
  expect_type(bp.loo, "list")
  expect_named(
    bp.loo,
    c(
      "dimx",
      "dimy",
      "n.axes",
      "press0",
      "CVfit",
      "varianceExp",
      "totalVar",
      "call",
      "nam.dat"
    )
  )
  expect_output(
    print(bp.loo),
    regexp = "Cross-validation for Predictive Co-Correspondence Analysis"
  )
})
