#' Calculate the potential number of ties in a network
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
potential_ties <- function(x, ...) {
  UseMethod("potential_ties")
}

#' @param n_nodes
#' @param directed
#'
#' @return
#' @export
#'
#' @examples
potential_ties.double <- function(n_nodes, directed = TRUE) {
  pt_directed <- (n_nodes*(n_nodes-1))

  if(directed) {
    return(pt_directed)
  } else {
    pt_undirected <- pt_directed/2
    return(pt_undirected)
  }
}


#'Turn an edge list into a matrix
#'
#' @name edge_list_to_matrix
#'
#' @param el edge list to turn into a matrix. The edge list needs to be
#' formatted such that
#' \itemize{
#'   \item the first column is named "source_name"
#'   \item the second column is named "target_name"
#'   \item the third column is named "link_value"
#' }
#'
#' @return a matrix
#' @export
#'
#' @importFrom tidyr conplete spread
#' @import dplyr
edge_list_to_matrix <- function(el) {
  el_cols <- colnames(el)

  if(el_cols[1] != "source_name" |
     el_cols[2] != "target_name" |
     el_cols[3] != "link_value") {
    stop("Expects first column to be named 'source_name',
         second column to be named 'target_name',
         third column to be named 'link_value'")
  }

  all_nodes <- unique(c(el[["source_name"]], el[["target_name"]]))

  mat_tbl <-
    tidyr::complete(el,
                    source_name = all_nodes,
                    target_name = all_nodes) %>%
    tidyr::spread(target_name, link_value)

  source_names <- mat_tbl[["source_name"]]

  mat <-
    mat_tbl %>%
    select(-source_name) %>%
    as.matrix

  rownames(mat) <- source_names

  mat
}

