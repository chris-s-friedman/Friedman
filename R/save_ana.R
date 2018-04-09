#' Save Analytical Data
#'
#' Saves analytical data in all the forms used by the Hassrick Lab.
#'
#' @param x data frame, tibble, some data
#' @param file_name location to save to (including the file name). Do not
#'   include the file extension. That is handled internally.
#'
#' @importFrom glue glue
#' @importFrom readr write_csv
#' @importFrom haven write_sav write_sas write_dta
#'
#' @return NULL invisibly
#' @export
save_ana <- function(x, file_name) {
  readr::write_csv(x, path = glue(file_name, ".csv"))
  haven::write_sav(x, path = glue(file_name, ".sav"))
  haven::write_sas(x, path = glue(file_name, ".sas7bdat"))
  haven::write_dta(x, path = glue(file_name, ".dta"))
  saveRDS(x, file = glue(file_name, ".Rds"))
}
