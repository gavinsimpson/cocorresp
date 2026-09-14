# Run from the package root after installing this checkout.
# Rscript benchmarks/parallel.R > /tmp/cocorresp-benchmark.csv
library(cocorresp)

benchmark_parallel <- function() {
  set.seed(20260913)
  cases <- list()
  data(beetles, plants, envir = environment())
  cases$beetles <- list(y = log1p(beetles), x = plants)
  cases$synthetic <- list(
    y = matrix(rexp(80 * 30), 80, 30),
    x = matrix(rexp(80 * 35), 80, 35)
  )
  startup <- system.time(cl <- parallel::makePSOCKcluster(2))[["elapsed"]]
  on.exit(parallel::stopCluster(cl), add = TRUE)
  out <- list(data.frame(
    case = "worker-startup",
    operation = "PSOCK",
    workers = 2,
    seconds = startup
  ))
  for (nm in names(cases)) {
    d <- cases[[nm]]
    d$x <- d$x[, colSums(d$x) > 0, drop = FALSE]
    d$y <- d$y[, colSums(d$y) > 0, drop = FALSE]
    fit <- coca(d$y, d$x, n.axes = 3, quiet = TRUE)
    for (workers in c(1L, 2L)) {
      backend <- if (workers == 1L) 1L else cl
      for (operation in c("LOO", "permutation")) {
        elapsed <- replicate(3, {
          set.seed(42)
          system.time({
            if (operation == "LOO") {
              crossval(
                d$y,
                d$x,
                n.axes = 3,
                verbose = FALSE,
                parallel = backend
              )
            } else {
              vegan::permutest(
                fit,
                permutations = 49,
                verbose = FALSE,
                parallel = backend
              )
            }
          })[["elapsed"]]
        })
        out[[length(out) + 1L]] <- data.frame(
          case = nm,
          operation = operation,
          workers = workers,
          seconds = median(elapsed)
        )
      }
    }
  }
  do.call(rbind, out)
}
write.csv(benchmark_parallel(), stdout(), row.names = FALSE)
