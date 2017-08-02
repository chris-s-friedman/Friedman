#' Build project directories quickly and easily
#'
#' When I start a new project, I always start by building my shell project
#' directory. Every project I work in has 5 folders:
#' \enumerate{
#'   \item \strong{R}, where all of my written code lives.
#'   \item \strong{figs}, where all of the figures produced go.
#'   \item \strong{output}, where all of the data that I have created or
#'   manipulated goes. Treat things placed in this folder as volatile and
#'   non-permenent. It is often that as a project progresses, data written to
#'   this folder will be overwritten.
#'   \item \strong{docs}, where all of my guiding documents and non-R related
#'   items tend to go.
#'   \item \strong{data}, where raw data lives. I treat this as read-only.
#'   Depending on the project, this folder may be empty and data may be read in
#'   from another source (such as a USB drive or encrypted drive)
#'   }
#'
#' @param projectpath Points to the directory where the sub-directories should
#' be placed. Defaults to the current working directory.
#'
#' @author Chris Friedman, \email{chris.s.friedman@@gmail.com}
#'
#' @examples InitializeR()
#'
#' @export
InitializeR <- function(projectpath = "./"){
  dir.create(paste(projectpath, "R", sep = "/"))
  dir.create(paste(projectpath, "figs", sep = "/"))
  dir.create(paste(projectpath, "output", sep = "/"))
  dir.create(paste(projectpath, "docs", sep = "/"))
  dir.create(paste(projectpath, "data", sep = "/"))
}
