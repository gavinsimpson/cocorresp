test_that("summary.symcoa() works & with default args", {
  data(beetles, plants)
  bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric", quiet = TRUE)
  expect_silent(summ <- summary(bp.sym))
  expect_s3_class(summ, "summary.symcoca")
  expect_named(summ, c("inertia", "lambda", "call"))
})

test_that("summary.predcoca() works & with default args", {
  data(beetles, plants)
  bp.pred <- coca(beetles ~ ., data = plants, quiet = TRUE)
  expect_silent(summ <- summary(bp.pred))
  expect_s3_class(summ, "summary.predcoca")
  expect_named(
    summ,
    c(
      "cocaScores",
      "call",
      "lambda",
      "namY",
      "namX",
      "loadings",
      "varianceExp",
      "totalVar"
    )
  )
})
