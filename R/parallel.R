#' @noRd
make_mapper <- function(parallel) {
  owned <- FALSE
  cl <- NULL
  if (is.function(parallel)) {
    apply <- parallel
  } else if (inherits(parallel, "cluster")) {
    cl <- parallel
  } else {
    validate_count(parallel, "parallel")
    if (parallel > 1L) {
      cl <- parallel::makePSOCKcluster(parallel)
      owned <- TRUE
    }
  }
  if (!is.function(parallel)) {
    apply <- if (is.null(cl)) {
      lapply
    } else {
      function(X, FUN, ...) {
        parallel::parLapply(cl, X, FUN, ...)
      }
    }
  }
  list(
    map = function(X, FUN, ...) {
      out <- apply(X, run_task, worker = FUN, .task_args = list(...))
      if (
        !is.list(out) ||
          length(out) != length(X) ||
          !all(vapply(
            seq_along(X),
            function(i) {
              is.list(out[[i]]) &&
                identical(out[[i]]$index, X[[i]]) &&
                "result" %in% names(out[[i]])
            },
            logical(1)
          ))
      ) {
        stop("parallel mapping function must return a list in input order")
      }
      lapply(out, `[[`, "result")
    },
    close = function() {
      if (owned) {
        parallel::stopCluster(cl)
      }
      invisible(NULL)
    }
  )
}

#' @noRd
run_task <- function(i, worker, .task_args = list()) {
  result <- tryCatch(
    do.call(worker, c(list(i), .task_args)),
    error = function(e) {
      stop("Task ", i, ": ", conditionMessage(e), call. = FALSE)
    }
  )
  list(index = i, result = result)
}
