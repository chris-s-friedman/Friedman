#' Save Analytical Data
#'
#' Saves analytical data in all the forms used by the Hassrick Lab.
#'
#' @param x data frame, tibble, some data
#' @param file_name location to save to (including the file name). Do not
#'   include the file extension. That is handled internally.
#' @param file_types what file types should be output?
#'
#' @importFrom glue glue
#' @importFrom readr write_csv
#' @importFrom haven write_sav write_sas write_dta
#'
#' @return NULL invisibly
#' @export
save_ana <- function(x, file_name,
                     file_types = c("csv", "spss", "sas", "stata", "r")) {
  if("csv" %in% file_types) {
    readr::write_csv(x, path = glue(file_name, ".csv"))
    }
  if("spss" %in% file_types) {
    haven::write_sav(x, path = glue(file_name, ".sav"))
    }
  if("sas" %in% file_types) {
    haven::write_sas(x, path = glue(file_name, ".sas7bdat"))
    }
  if("stata" %in% file_types) {
    haven::write_dta(x, path = glue(file_name, ".dta"))
    }
  if("r" %in% file_types)  {
    saveRDS(x, file = glue(file_name, ".Rds"))
    }
}
