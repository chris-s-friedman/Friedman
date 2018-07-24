#' Make Advanced Tables
#'
#' Advanced Tables are a "long" way to look at network data where every node has
#' columns about their attributes. Additionally, all of the nodes and attributes
#' carry their own meta-data. This is a form that is made to make simple import
#' into ORA.
#'
#' @name adv_tbl
#' @aliases is_adv_tbl
#'
#' @param attributes A data frame with the atributes ceated by \code{\link{as_adv_attr()}}
#' @param edges A data frame with the edges ceated by \code{\link{as_adv_edge()}}
#' @param directed Logical scalar - is the data directed (currently unused)
#' @param group_col column to group the data by
#'
#' @return an advanced table
#' @export
#'
#' @examples
#'
#' @importFrom tidyr gather spread
#' @import dplyr
adv_tbl <- function(attributes, edges, directed = TRUE, group_col = NULL) {
  group_col <- enquo(group_col)
  group_col_name <- quo_name(group_col)
  if(!"adv_tbl_attr" %in% class(attributes)) {
    stop("coerce attributes to adv_tbl_attr with as_adv_attr()")
  }
  if(!"adv_tbl_edge" %in% class(edges)) {
    stop("coerce edges to adv_tbl_edge with as_adv_edge()")
  }
  attributes <- attributes %>%
    mutate(col_name_safe = attr_name) %>%
    gather(variable, value, attr_type, contains("attr")) %>%
    unite(colname, col_name_safe, variable) %>%
    spread(colname, value)
  source_attr <-
    attributes %>%
    rename_at(vars(-group_col_name), funs(paste0("source_", .)))
  targ_attr <-
    attributes %>%
    rename_at(vars(-group_col_name), funs(paste0("target_", .)))
  edges %>%
    left_join(source_attr, by = c("source_node_name", group_col_name)) %>%
    left_join(targ_attr, by = c("target_node_name", group_col_name)) %>%
    set_adv_tbl_class() %>%
    select(source_nodeset_class, source_nodeset_name, source_node_name,
           target_nodeset_class, target_nodeset_name, target_node_name,
           everything())
}

#' @rdname is_adv_tbl
#' @export
#' @param x An object to test for class "adv_tbl"
is_adv_tbl <- function(x) {
  "adv_tbl" %in% class(x)
}



