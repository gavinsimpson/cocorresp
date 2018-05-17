## Test `summary()` methods

## load packages
library("testthat")
library("cocorresp")

context("Testing summary() methods")

## Load data
data(beetles, plants)
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric",
               quiet = TRUE)

test_that("summary.symcoa() works & with default args", {
    expect_silent(summ <- summary(bp.sym))
    expect_is(summ, "summary.symcoca")
    expect_named(summ, c("inertia", "lambda", "call"))
})

## Load data
data(beetles, plants)
bp.pred <- coca(beetles ~ ., data = plants, quiet = TRUE)

test_that("summary.predcoca() works & with default args", {
    expect_silent(summ <- summary(bp.pred))
    expect_is(summ, "summary.predcoca")
    expect_named(summ, c("cocaScores", "call", "lambda", "namY", "namX",
                         "loadings", "varianceExp", "totalVar"))
})
