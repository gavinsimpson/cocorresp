test_that("matrix and formula interfaces agree and validate inputs", {
  d <- community_fixture()
  for (method in c("predictive", "symmetric")) {
    a <- coca(d$y, d$x, method = method, n.axes = 2)
    y <- d$y
    b <- coca(y ~ ., as.data.frame(d$x), method = method, n.axes = 2)
    expect_identical(class(a), class(b))
    expect_equal(numeric_result(a), numeric_result(b))
  }
  expect_s3_class(coca(d$y ~ d$x, n.axes = 2), "predcoca")
  expect_error(coca(d$y ~ 1), "numeric matrix")
  expect_error(coca(d$y, d$x[-1, ]), "rows")
  for (bad in list(NA, Inf, 0, -1, 1.5, c(1, 2), "two")) {
    expect_error(coca(d$y, d$x, n.axes = bad), "positive integer")
  }
  for (bad in list(
    -d$y,
    replace(d$y, 1, NA),
    replace(d$y, 1, Inf),
    matrix("a", 3, 3)
  )) {
    expect_error(coca(bad, d$x), "finite, non-negative")
  }
  expect_error(checkCommunityData(1:5), "matrix")
  expect_error(checkCommunityData(matrix(1, 1, 3)), "two rows")
  expect_error(
    checkCommunityData(matrix(c(1, 0, 1, 0), 2, byrow = TRUE)),
    "non-empty"
  )
  z <- d$y
  z[1, ] <- 0
  expect_error(coca(z, d$x), "row sums")
  z <- d$y
  z[, 1] <- 0
  expect_message(coca(z, d$x, n.axes = 2), "Removed")
  expect_silent(coca(z, d$x, n.axes = 2, quiet = TRUE))
  for (w in list(1, rep(0, 16), rep(NA_real_, 16), -seq_len(16))) {
    expect_error(coca(d$y, d$x, weights = w), "weights")
  }
  expect_warning(coca(d$y, d$x, n.axes = 100), "reset")
  expect_warning(coca(d$y, d$x, method = "symmetric", n.axes = 100), "reset")
  expect_warning(coca(d$y, d$x, reg.method = "eigen", n.axes = 100), "reset")
})

test_that("eigen fitting computes all requested axes for both width branches", {
  d <- community_fixture()
  for (pair in list(list(d$y, d$x), list(d$x, d$y))) {
    fit <- coca(pair[[1]], pair[[2]], reg.method = "eigen", n.axes = 3)
    expect_true(all(fit$lambda > 0))
    expect_true(all(colSums(abs(fit$scores$species$Y)) > 0))
    expect_true(all(is.finite(unlist(fit$scores))))
    fit1 <- coca(pair[[1]], pair[[2]], reg.method = "eigen", n.axes = 1)
    expect_equal(unname(fit$lambda[1]), unname(fit1$lambda))
  }
})

test_that("weighted transformations agree with explicit matrix algebra", {
  d <- community_fixture()
  y <- d$y
  w <- seq_len(nrow(y))
  k <- colSums(y) / sum(y)
  q <- diag(1 / rowSums(y)) %*% y %*% diag(1 / k) - 1
  expected <- diag(sqrt(w / sum(w))) %*% q %*% diag(sqrt(k))
  expect_equal(unname(mcChi(y, w)$Ychi), unname(expected))
  expect_equal(unname(scaleChi(y, k, w / sum(w))), unname(expected))
  for (eps in c(0.5e-6, 1e-6, 2e-6)) {
    ysmall <- matrix(c(eps, 1 - eps, eps, 1 - eps), 2, byrow = TRUE)
    expect_true(all(is.finite(mcChi(ysmall, c(1, 1))$Ychi)))
    expect_true(all(is.finite(scaleChi(ysmall, c(eps, 1 - eps), c(0.5, 0.5)))))
  }
  lin <- mcLin(y, w)
  expect_equal(
    colSums(lin$rXs * sqrt(w / sum(w))),
    rep(0, ncol(y)),
    ignore_attr = TRUE,
    tolerance = 1e-8
  )
  expect_equal(
    unname(scaleLin(y, lin$mean, lin$sd, w / sum(w), eps = 1e-9)),
    unname(lin$rXs)
  )
})

test_that("SIMPLS agrees with the independent pls implementation", {
  d <- community_fixture()
  x <- mcChi(d$x, rowSums(d$y))$Ychi
  y <- mcChi(d$y, rowSums(d$y))$Ychi
  fit <- simpls(x, y, 3)
  ref <- pls::simpls.fit(x, y, 3, center = FALSE)
  expect_equal(
    unname(fit$coefficients),
    unname(ref$coefficients),
    tolerance = 1e-8
  )
  stripped <- simpls(x, y, 3, stripped = TRUE)
  expect_equal(unname(fit$coefficients), unname(stripped$coefficients))
  expect_equal(fit$Xtotvar, sum(x^2))
  expect_equal(fit$Ytotvar, sum(y^2))
  for (j in 1:3) {
    expect_equal(
      unname(fit$coefficients[,, j]),
      unname(
        fit$projection[, 1:j, drop = FALSE] %*%
          t(fit$Yloadings[, 1:j, drop = FALSE])
      )
    )
  }
  expect_true(all(is.finite(simpls(unname(x), unname(y), 1)$coefficients)))
})

test_that("symmetric fitting conserves inertia and symmetric weights commute", {
  d <- community_fixture()
  a <- coca(d$y, d$x, method = "symmetric", symmetric = TRUE, n.axes = 2)
  b <- coca(d$x, d$y, method = "symmetric", symmetric = TRUE, n.axes = 2)
  expect_equal(a$lambda, b$lambda)
  expect_equal(
    weights(a),
    (rowSums(d$y) + rowSums(d$x)) / (sum(d$y) + sum(d$x)),
    ignore_attr = TRUE
  )
  for (block in c("Y", "X")) {
    total <- a$inertia$total[[block]]
    residual <- a$inertia$residual[[block]]
    expect_gte(total, residual - 1e-12)
  }
  fit <- coca(d$y, d$x, method = "symmetric", weights = seq_len(16), n.axes = 2)
  expect_equal(weights(fit), seq_len(16) / sum(seq_len(16)))
  pred <- coca(d$y, d$x, weights = seq_len(16), n.axes = 2)
  expect_equal(weights(pred), seq_len(16))
  co <- coinertia(d$y, d$x, n.axes = 2, symmetric = TRUE)
  expect_equal(co$lambda, a$lambda, ignore_attr = TRUE)
  expect_warning(coinertia(d$y, d$x, n.axes = 100), "reset")
  expect_s3_class(
    coinertia(d$y, d$x, weights = seq_len(16), n.axes = 1),
    "coinertia"
  )
  slow <- coinertiaI(d$y, d$x, fast = FALSE)
  expect_s3_class(slow, "coinertiaI")
  expect_equal(unname(slow$lambda), diag(svd(crossprod(d$y, d$x))$d^2))
  expect_equal(unname(coinertiaI(d$y, d$x)), unname(slow$scores$Y))
  residual <- residual_matrix(d$y, cbind(1, d$x[, 1]))
  expect_equal(
    residual$inertia$total,
    residual$inertia$fitted + residual$inertia$residual
  )
  expect_error(residual_matrix(d$y, d$x[-1, ]), "rows")
})

test_that("cross-validation handles one axis and is invariant to row order", {
  d <- community_fixture()
  a <- crossval(d$y, d$x, n.axes = 1, verbose = FALSE)
  b <- crossval(d$y[16:1, ], d$x[16:1, ], n.axes = 1, verbose = FALSE)
  expect_equal(numeric_result(a), numeric_result(b), tolerance = 1e-8)
  fit <- simpls(
    mcChi(d$x, rowSums(d$y))$Ychi,
    mcChi(d$y, rowSums(d$y))$Ychi,
    1,
    stripped = TRUE
  )
  expect_equal(a$varianceExp, list(Xblock = fit$Xvar, Yblock = fit$Yvar))
  expect_equal(a$totalVar, list(Xblock = fit$Xtotvar, Yblock = fit$Ytotvar))
  expect_error(crossval(d$y, d$x, n.axes = 99), "PLS axes")
  expect_error(crossval(d$y[1:2, ], d$x[1:2, ], n.axes = 1), "three rows")
  expect_snapshot(crossval(d$y, d$x, n.axes = 1))
})

test_that("internal fitters handle direct calls and degenerate data explicitly", {
  d <- community_fixture()
  expect_s3_class(predcoca.simpls(d$y, d$x, n.axes = 2), "predcoca")
  expect_s3_class(predcoca.eigen(d$y, d$x, n.axes = 2), "predcoca")
  expect_s3_class(symcoca(d$y, d$x, n.axes = 2), "symcoca")
  expect_error(symcoca(d$y, d$x[-1, ], n.axes = 2), "rows")
  expect_equal(residuals(model_fixture()), resid.symcoca(model_fixture()))
  expect_error(simpls(d$x, d$y, 0), "positive integer")
  expect_error(simpls(d$x, d$y, 20), "dimensions")
  expect_error(simpls("bad", d$y, 1), "numeric matrix")
  expect_error(simpls(d$x, d$y[-1, ], 1), "same rows")
  expect_error(simpls(matrix(0, 8, 3), matrix(0, 8, 2), 1), "zero covariance")
  rank_one <- outer(seq_len(8), c(1, 2, 3))
  expect_true(all(is.finite(
    simpls(rank_one, rank_one[, 1, drop = FALSE], 1)$coefficients
  )))
})
