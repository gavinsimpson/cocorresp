test_that("symmetric biplot appearance is unchanged", {
  # Preserve the reference when visual comparisons are skipped.
  testthat::announce_snapshot_file("symmetric-biplot.svg")
  skip_if(
    Sys.getenv("COCORRESP_VISUAL_TESTS") != "true",
    "Visual tests are opt-in"
  )
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("symmetric biplot", function() {
    biplot(model_fixture())
  })
})

test_that("predictive biplot appearance is unchanged", {
  # Preserve the reference when visual comparisons are skipped.
  testthat::announce_snapshot_file("predictive-biplot.svg")
  skip_if(
    Sys.getenv("COCORRESP_VISUAL_TESTS") != "true",
    "Visual tests are opt-in"
  )
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("predictive biplot", function() {
    biplot(model_fixture("predictive"))
  })
})
