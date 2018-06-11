## Test `eigenvals()` methods

## load packages
library("testthat")
library("cocorresp")

context("Testing eigenvals() methods")

## Load data
data(beetles, plants)
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric",
               quiet = TRUE)

test_that("eigenvals.symcoca() works & with default args", {
    expect_silent(ev <- eigenvals(bp.sym))
    expect_equal(length(ev), length(bp.sym$lambda))
    expect_is(ev, "numeric")
})

test_that("eigenvals.symcoca() works with choices", {
    expect_silent(ev <- eigenvals(bp.sym, choices = 1:2))
    expect_equal(length(ev), 2L)
    expect_is(ev, "numeric")
})
