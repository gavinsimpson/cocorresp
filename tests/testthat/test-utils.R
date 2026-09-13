test_that("eigenvals() works", {
  data(beetles, plants, package = "cocorresp")
  beetles <- log(beetles + 1) # log transform the bettle data
  bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric", quiet = TRUE)
  expect_silent(ev <- eigenvals(bp.sym))
  expect_type(ev, "double")
  expect_named(ev, paste("COCA", seq_along(ev)))
  expect_length(ev, nrow(beetles) - 1L)
  expect_length(ev, bp.sym$n.axes)
})

test_that("corAxis() works", {
  data(beetles, plants, package = "cocorresp")
  beetles <- log(beetles + 1) # log transform the bettle data
  bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric", quiet = TRUE)
  expect_silent(corels <- corAxis(bp.sym))
  expect_type(corels, "double")
  expect_named(corels, paste("COCA", seq_along(corels)))
  expect_length(corels, nrow(beetles) - 1L)
  expect_length(corels, bp.sym$n.axes)

  expect_error(corAxis(1L:10L), regexp = "No default method for corAxis")
})
