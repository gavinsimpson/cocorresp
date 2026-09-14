test_that("public vegan generics are re-exported unchanged", {
  for (name in c("scores", "eigenvals", "permutest", "envfit")) {
    expect_identical(
      getExportedValue("cocorresp", name),
      getExportedValue("vegan", name)
    )
  }
  expect_false(any(
    c("pasteCall", "vectorfit", "factorfit") %in%
      getNamespaceExports("cocorresp")
  ))
})
