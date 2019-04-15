potential_ties <- function(n_nodes, directed = TRUE) {
  pt_directed <- (n_nodes*(n_nodes-1))

  if(directed) {
    return(pt_directed)
  } else {
    pt_undirected <- pt_directed/2
    return(pt_undirected)
  }
}

library(dplyr)

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

