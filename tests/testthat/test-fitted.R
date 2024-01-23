## Test `coinertia()` function

## load packages
library("testthat")
library("cocorresp")

context("Testing fitted() methods")

## Load data
data(beetles, plants)
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric", quiet = TRUE)

test_that("fitted() works & with default args", {
    expect_silent(ff <- fitted(bp.sym))
    expect_is(ff, "fitted.symcoca")
    expect_named(ff, c("Y", "X", "nam.dat"))
    expect_equal(length(ff$nam.dat), 2L)
    expect_output(print(ff), regexp = "Fitted values for:")
})

test_that("fitted() works for a single 'which'", {
    expect_silent(ff <- fitted(bp.sym, which = "response"))
    expect_is(ff, "fitted.symcoca")
    expect_named(ff, c("Y", "nam.dat"))
    expect_equal(length(ff$nam.dat), 1L)
    expect_equal(unname(ff$nam.dat[1]), "beetles")
    expect_output(print(ff), regexp = "Fitted values for: beetles")

    expect_silent(ff <- fitted(bp.sym, which = "predictor"))
    expect_is(ff, "fitted.symcoca")
    expect_named(ff, c("X", "nam.dat"))
    expect_equal(length(ff$nam.dat), 1L)
    expect_equal(unname(ff$nam.dat[1]), "plants")
    expect_output(print(ff), regexp = "Fitted values for: plants")
})
