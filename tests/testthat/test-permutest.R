test_that("permutest() works", {
  withr::local_seed(42)
  data(beetles, plants, package = "cocorresp")
  beetles <- log(beetles + 1) # log transform the bettle data
  expect_message(
    bp.pred <- coca(beetles ~ ., data = plants),
    regexp = "Removed some species that contained no data in"
  )
  bp.perm <- permutest(bp.pred, permutations = 9, n.axes = 2, verbose = FALSE)
  expect_s3_class(bp.perm, "permutest.coca")
  expect_named(
    bp.perm,
    c(
      "pval",
      "permstat",
      "total.inertia",
      "inertia",
      "fitax",
      "pcent.fit",
      "n.axes",
      "call"
    )
  )
  expect_output(
    print(bp.perm),
    regexp = "Permutation test for predictive co-correspondence analysis:"
  )
})
