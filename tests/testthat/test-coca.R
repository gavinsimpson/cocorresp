## Test `coca()` function

## load packages
library("testthat")
library("cocorresp")

context("Testing coca()")

## Load data
data(beetles)
beetles <- log(beetles + 1)            # log transform the bettle data
data(plants)

test_that("coca() works & returns correct object", {
    ## symmetric CoCA
    bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")
    expect_is(bp.sym, "coca")
    expect_is(bp.sym, "symcoca")
})
