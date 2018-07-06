missing_arg_handler <- function() {
  defined <- ls(name = parent.frame())
  passed <- names(as.list(match.call(definition = sys.function(- 1),
                                     call = sys.call(- 1),
                                     expand.dots = TRUE
  ))[-1])
  if (any(!defined %in% passed)) {
    stop(paste("missing values for", paste(setdiff(defined, passed), collapse=", ")))
  }
}
