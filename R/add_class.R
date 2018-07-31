#' Add a class to an object
#'
#' Adds a class to an object.
#'
#' @param x the object to add a class to
#' @param new_class the new class or classes
#' @param location should the new class come before the old classes or after
#' them?
#'
#' @return the object, but with new classes
#' @export
#'
#' @examples
#' a <- "Hello world!"
#' class(a)
#' b <- add_class(a, "awesome_text")
#' class(b)
#'
#' # You can also add multiple classes to an object at once!
#' c <- add_class(a, c("class1", "class2"))
#' class(c)
add_class <- function(x, new_class, location = "first") {
  if(location == "first") class(x) <- c(new_class, class(x))
  if(location == "last")  class(x) <- c(class(x), new_class)
  x
}
