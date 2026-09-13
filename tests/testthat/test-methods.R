test_that("extractors preserve intended blocks and dimensions", {
  for (method in c("symmetric", "predictive")) {
    fit <- model_fixture(method)
    expect_length(weights(fit), 16)
    for (which in list("response", "predictor", c("y1", "y2"))) {
      out <- loadings(fit, which = which)
      expect_true(is.matrix(out) || is.list(out))
    }
    expect_equal(
      scores(fit, choices = 1)$species$Y,
      fit$scores$species$Y[, 1, drop = FALSE]
    )
    expect_length(scores(fit, display = "sites"), 1)
    expect_equal(
      loadings(fit, which = "response", choices = 1),
      fit$loadings$Y[, 1]
    )
    expect_length(eigenvals(fit, choices = integer()), 0)
  }
  s <- model_fixture()
  expect_equal(
    scores(s, display = "loadings")$loadings$X,
    s$loadings$X[, 1:2, drop = FALSE]
  )
  expect_equal(scores(s, display = "xmatrix")$xmatrix, s$X[, 1:2, drop = FALSE])
  expect_equal(resid(s), s$residuals)
  expect_equal(
    rescale(s, choices = 1)$species$Y,
    s$scores$species$Y[, 1, drop = FALSE] * s$lambda[1]^0.25,
    ignore_attr = TRUE
  )
  expect_equal(
    scores(s, scaling = TRUE)$sites,
    rescale(s, choices = 1:2, display = "sites")
  )
  expect_equal(
    scores(s, scaling = TRUE)$species,
    rescale(s, choices = 1:2, display = "species")
  )
  expect_length(rescale(s), 2)
  expect_length(corAxis(s, axes = 1), 1)
  expect_error(corAxis(1), "default")
  expect_error(corAxis.symcoca(1), "symcoca")
  expect_error(rescale(1), "default")
  expect_error(scores.symcoca(1), "symcoca")
  expect_error(scores.predcoca(1), "predcoca")
  expect_error(loadings.symcoca(1), "symcoca")
  expect_error(loadings.predcoca(1), "predcoca")
  expect_equal(
    loadings(list(loadings = matrix(1, 2, 2))),
    stats::loadings(list(loadings = matrix(1, 2, 2)))
  )
  expect_identical(selectWhich(c("Y", "response", "X")), c("Y", "X"))
  expect_identical(selectWhich("unknown"), character())
})

test_that("print and summary methods dispatch for every supported class", {
  d <- community_fixture()
  models <- list(
    model_fixture(),
    model_fixture("predictive"),
    model_fixture("predictive", "eigen"),
    coinertia(d$y, d$x, n.axes = 2),
    crossval(d$y, d$x, n.axes = 2, verbose = FALSE),
    vegan::permutest(
      model_fixture("predictive"),
      permutations = 3,
      verbose = FALSE
    ),
    fitCoinertia(d$y, rep(0.2, 5), d$x, rep(1 / 6, 6), rep(1 / 16, 16), 2)
  )
  for (fit in models) {
    expect_output(print(fit), "[A-Za-z]")
    invisible(capture.output(expect_invisible(print(fit))))
    if (!inherits(fit, "coinertia")) {
      sm <- summary(fit)
      expect_output(print(sm), "[A-Za-z]")
      invisible(capture.output(expect_invisible(print(sm))))
      if (!inherits(fit, c("crossval", "permutest.coca"))) {
        expect_equal(eigenvals(sm), eigenvals(fit))
      }
    }
  }
  expect_snapshot(print(summary(model_fixture("predictive"))))
  for (which in c("response", "predictor")) {
    ff <- fitted(model_fixture(), which = which)
    expect_output(print(ff), "Fitted values")
    invisible(capture.output(expect_invisible(print(ff))))
  }
  expect_s3_class(eigenvals(models[[7]]), "eigenvals")
  expect_length(eigenvals(models[[7]], choices = 1), 1)
})

test_that("plots return correct coordinates and respect scaling", {
  withr::local_pdf(tempfile(fileext = ".pdf"))
  for (fit in list(
    model_fixture(),
    model_fixture("predictive"),
    model_fixture("predictive", "eigen")
  )) {
    for (type in c("text", "points", "none")) {
      out <- plot(fit, type = type)
      expect_s3_class(out, "ordiplot")
      expect_equal(out$species, scores(fit)$species$Y)
    }
    expect_s3_class(
      plot(
        fit,
        display = "sites",
        xlab = "x",
        ylab = "y",
        xlim = c(-10, 10),
        ylim = c(-10, 10),
        axes = FALSE,
        ann = FALSE
      ),
      "ordiplot"
    )
    expect_error(plot(fit, choices = 1), "two axes")
    expect_error(plot(fit, which = "bad"), "Unknown")
    expect_message(plot(fit, which = c("response", "predictor")), "single")
    if (is.null(fit$method) || fit$method != "eigen") {
      for (which in c("response", "predictor")) {
        expect_s3_class(biplot(fit, which = which, type = "points"), "ordiplot")
        expect_s3_class(biplot(fit, which = which), "ordiplot")
      }
    }
    expect_error(biplot(fit, choices = 1), "two axes")
    expect_error(biplot(fit, which = "bad"), "exactly one")
    plot(fit)
    expect_equal(
      unname(points(fit, display = "sites", which = "predictor")),
      unname(scores(fit)$sites$X)
    )
    expect_warning(points(fit), "Only one")
    expect_error(points(fit, choices = 1, display = "sites"), "two axes")
  }
  fit <- model_fixture()
  expect_equal(
    plot(fit, scaling = TRUE)$sites,
    scores(fit, scaling = TRUE)$sites$Y
  )
  expect_s3_class(biplot(fit, benzecri = FALSE), "ordiplot")
  expect_silent(screeplot(fit))
  expect_silent(screeplot(fit, xlab = "x", ylab = "y"))
  expect_error(draw_coca(list(), NULL, 1:2), "No scores")
  big <- list(species = matrix(seq_len(200), 100, 2))
  expect_s3_class(draw_coca(big, NULL, 1:2), "ordiplot")
  vdiffr::expect_doppelganger("symmetric biplot", function() biplot(fit))
  vdiffr::expect_doppelganger("predictive biplot", function() {
    biplot(model_fixture("predictive"))
  })
})
