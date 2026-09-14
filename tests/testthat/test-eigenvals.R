test_that("eigenvals.symcoca() works & with default args", {
  data(beetles, plants)
  bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric", quiet = TRUE)
  expect_silent(ev <- eigenvals(bp.sym))
  expect_length(ev, length(bp.sym$lambda))
  expect_s3_class(ev, "eigenvals")
  expect_type(ev, "double")
})

test_that("eigenvals.symcoca() works with choices", {
  data(beetles, plants)
  bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric", quiet = TRUE)
  expect_silent(ev <- eigenvals(bp.sym, choices = 1:2))
  expect_length(ev, 2L)
  expect_s3_class(ev, "eigenvals")
  expect_type(ev, "double")
})
