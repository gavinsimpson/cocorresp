#' @rdname crossval
#' @export
summary.crossval <-
  function(object, axes = seq_len(min(6, object$n.axes)), ...) {
    class(object) <- "summary.crossval"
    object
  }
