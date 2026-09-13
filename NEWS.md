# cocorresp 0.5.0

## Development and documentation

* Require R >= 4.1 and use testthat edition 3.
* Generate help and NAMESPACE from Markdown roxygen comments.
* Add numerical regression tests, graphical snapshots, and modern CI/site workflows.
* Export the documented `loadings()` generic, forwarding non-CoCA objects to stats.
* Move vegan from Depends to Imports. Attaching cocorresp no longer attaches
  vegan; use `vegan::scores()`, `vegan::eigenvals()`, `vegan::permutest()` and
  `vegan::envfit()`, or explicitly attach vegan in existing scripts.
* Guard each optional parallel-backend test with `skip_if_not_installed()`;
  test future and futurize independently.

## Correctness fixes

* Compute every requested eigen axis when the response has at least as many species as the predictor.
* Support one-axis cross-validation; derive variance summaries from the full-data fit and correctly report response-block variance. These summaries may change from earlier versions.
* Preserve matrix dimensions when extracting symmetric predictor loadings.
* Fix the chi-square scaling floor at exactly epsilon and vector scaling in `scaleLin()`.
* Respect `scaling = FALSE` in plots, points, and environment fitting. Default symmetric plots may consequently change.
* Handle formula responses in local environments and remove duplicate class entries from the matrix interface.
* Align environment data, weights, strata, and supported permutation designs after missing-value removal; omit spurious `na.action` metadata.
* Register summary printers; fix variance formatting, stale co-inertia field references, and eigenvalue printing.
* Return the intended result object from the non-fast internal co-inertia calculation.
* Validate community data, weights, axis counts, and worker specifications before computation.

## Parallel computation

* Add serial-by-default `parallel` arguments accepting worker counts, caller-owned clusters, or ordered mapping functions.
* Support PSOCK and mirai clusters plus documented future/futurize adapters.
* Generate random permutations centrally, preserving legacy integer-count sampling and RNG behavior across backends.
* Accept restricted permutation controls and explicit permutation matrices.
* Report progress through messages rather than stdout; `verbose = FALSE` remains silent for progress.

## Internal simplification

* Share input validation and dispatch between formula/matrix interfaces.
* Use row/column scaling for chi-square transformations and share plotting machinery.
* Aggregate LOO errors once and isolate permutation workers and residualization.
