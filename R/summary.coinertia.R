#' @rdname fitCoinertia
#' @export
"summary.fitCoinertia" <-
function(object, ...)
  {
    class(object) <- "summary.fitCoinertia"
    object
  }

