#' Convert adv_tbl in an igraph object
#'
#' Converts advanced format network data (a la ORA) into an igraph object
#'
#' @param adv_tbl adv_tbl data created by \code{\link{adv_tbl()}}
#'
#' @return an igraph graph object
#'
#' @author Chris Friedman, \email{chris.s.friedman@@gmail.com}
#' @seealso
#'
#' @examples graph_from_adv_tbl(adv_tbl)
#'
#' @import  dplyr
#' @importFrom tidyr spread
#' @importFrom igraph graph_from_data_frame
#'
#' @export
#'
graph_from_adv_tbl <- function(adv_tbl){
  if(!"adv_tbl" %in% class(adv_tbl)) {
    stop("Input is not an adv_tbl")
  }
  # create the edge list
  adv_edge <- as_adv_edge(adv_tbl)
  # create the vertex list
  adv_vert <- as_adv_attr(adv_tbl) %>%
    select(-attr_type) %>%
    spread(attr_name, attr_value) %>%
    select(node_name, everything())
  # make the igraph object
  graph_from_data_frame(d = adv_edge,
                                directed = TRUE,
                                vertices = adv_vert)
}
