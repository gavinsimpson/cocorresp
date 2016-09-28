## Test `coca()` function

## load packages
library("testthat")
library("cocorresp")

context("Testing utility functions")

## Load data
data(beetles, plants, package = "cocorresp")
beetles <- log(beetles + 1)            # log transform the bettle data
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")

test_that("eigenvals() works", {
    expect_silent(ev <- eigenvals(bp.sym))
    expect_is(ev, "numeric")
    expect_named(ev, paste("COCA", seq_along(ev)))
    expect_length(ev, nrow(beetles) - 1L)
    expect_length(ev, bp.sym$n.axes)
})

test_that("corAxis() works", {
    expect_silent(corels <- corAxis(bp.sym))
    expect_is(corels, "numeric")
    expect_named(corels, paste("COCA", seq_along(corels)))
    expect_length(corels, nrow(beetles) - 1L)
    expect_length(corels, bp.sym$n.axes)

    expect_error(corAxis(1L:10L),
                 regexp = "No default method for corAxis")
})
