test_that("coca() works & returns correct object", {
  data(beetles)
  beetles <- log(beetles + 1) # log transform the bettle data
  data(plants)
  data(bryophyte)
  data(vascular)
  ## symmetric CoCA
  expect_message(
    bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric"),
    regexp = "Removed some species that contained no data in:"
  )
  expect_s3_class(bp.sym, "coca")
  expect_s3_class(bp.sym, "symcoca")
  expect_type(bp.sym, "list")
  expect_named(
    bp.sym,
    c(
      "scores",
      "lambda",
      "X",
      "loadings",
      "residuals",
      "inertia",
      "rowsum",
      "colsum",
      "nam.dat",
      "n.axes",
      "weights",
      "call"
    )
  )
  expect_type(bp.sym$scores, "list")
  expect_output(print(bp.sym), regexp = "Symmetric Co-Correspondence Analysis")
  expect_silent(plot(bp.sym))

  ## predictive CoCA
  expect_silent(m <- coca(y = bryophyte, x = vascular, reg.method = "eigen"))
  expect_s3_class(m, "coca")
  expect_s3_class(m, "predcoca")
  expect_type(m, "list")
  expect_named(
    m,
    c(
      "nam.dat",
      "method",
      "n.axes",
      "lambda",
      "scores",
      "R0",
      "Ychi",
      "call",
      "loadings",
      "fitted",
      "varianceExp",
      "totalVar"
    )
  )
  expect_type(m$scores, "list")
  expect_output(print(m), regexp = "Predictive Co-Correspondence Analysis")
  expect_silent(plot(m))

  ## formula and SIMPLS
  expect_silent(
    m2 <- coca(bryophyte ~ ., data = vascular, reg.method = "simpls")
  )
  expect_s3_class(m2, "coca")
  expect_s3_class(m2, "predcoca")
  expect_type(m2, "list")
  expect_named(
    m2,
    c(
      "nam.dat",
      "call",
      "method",
      "scores",
      "loadings",
      "fitted",
      "varianceExp",
      "totalVar",
      "lambda",
      "n.axes",
      "Ychi",
      "R0"
    )
  )
  expect_type(m2$scores, "list")
  expect_output(print(m2), regexp = "Predictive Co-Correspondence Analysis")
  expect_silent(plot(m2))
})
