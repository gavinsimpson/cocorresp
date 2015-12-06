## Test `coinertia()` function

## load packages
library("testthat")
library("cocorresp")

context("Testing coinertia()")

## Load data
data(beetles, plants)

test_that("coinertia() works & returns correct object", {
    expect_message(coin <- coinertia(beetles, plants),
                   regexp = "some species contain no data")
    expect_is(coin, "coinertia")
    expect_is(coin, "list")
    expect_named(coin, c("scores", "weights", "lambda", "n.axes",
                         "symmetric", "call"))
    expect_output(print(coin), regexp = "Coinertia Analysis")
    expect_is(eigenvals(coin), "numeric")
    expect_equal(length(eigenvals(coin)), nrow(plants) - 1L)
})
