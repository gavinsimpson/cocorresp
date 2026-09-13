test_that("coinertia() works & returns correct object", {
  data(beetles, plants)
  expect_message(
    coin <- coinertia(beetles, plants),
    regexp = "some species contain no data"
  )
  expect_s3_class(coin, "coinertia")
  expect_type(coin, "list")
  expect_named(
    coin,
    c("scores", "weights", "lambda", "n.axes", "symmetric", "call")
  )
  expect_output(print(coin), regexp = "Coinertia Analysis")
  expect_type(eigenvals(coin), "double")
  expect_length(eigenvals(coin), nrow(plants) - 1L)
})

test_that("coinertia() fails with objects with different # samples", {
  data(beetles, plants)
  expect_error(
    coin <- coinertia(beetles, plants[-1, ]),
    regexp = "Number of rows in y and x is not equal"
  )
})
