#' Approximate string matching, stringr style
#'
#' Perform approximate string detection while using familiar stringr syntax.
#'   \code{str_detect_approx()} is a wrapper around base R \code{agrepl()}.
#'   \code{str_which_approx()} is a wrapper around base R \code{agrep()}.
#'   \code{str_subset_approx()} is a wrapper around
#'   \code{string[str_detect_approx(string, pattern)]}.
#'
#' @name str_detect_approx
#' @aliases str_which_approx
#' @aliases str_subset_approx
#'
#' @param string Input vector. Either a character vector, or something
#'   coearcible to one.
#' @param pattern Pattern to look for.
#' @param ... see the documentation for \code{\link[base]{agrep()}} for other
#'   arguments passed on to \code{agrep()} or \code{agrepl()}
#'
#' @return For \code{str_detect_approx()}, a logical vector. For
#'   \code{str_which_approx()} and \code{str_subset_approx()}, a character
#'   vector
#' @seealso agrep()
#' @export
#'
#' @examples
#' friends <- c("Jon", "Ron", "Jack", "Mac", "jan")
#' str_detect_approx(friends, "Jon")
#' str_detect_approx(friends, "Jon", ignore.case = TRUE)
str_detect_approx <- function(string, pattern, ...) {
  agrepl(pattern, string, ...)
}


#'
#' @rdname str_detect_approx
#' @export
str_which_approx <- function(string, pattern, ...) {
  agrep(pattern, string, ...)
}
#'
#' @rdname str_detect_approx
#' @export
str_subset_approx <-  function(string, pattern, ...) {
  string[str_detect_approx(string, pattern, ...)]
}
