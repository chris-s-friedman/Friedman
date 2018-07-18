#' Coerce object to adv_tbl
#'
#' @name as_adv_tbl
#'
#' @param x object to coerce to advanced table
#' @param ... aditional arguments to pass on to methods
#'
#' @return
#' @export
#'
#' @examples
as_adv_tbl <- function(x, ...) {
  UseMethod("as_adv_table")
}

#' @export
#' @rdname as_adv_tbl
as_adv_tbl.igraph <- function(x) {
  x %>%
    as_long_data_frame() %>%
    rename_all(funs(stringr::str_replace(., "^from", "source") %>%
                      stringr::str_replace(., "^to", "target")))
}

#' @export
#' @rdname as_adv_tbl
as_adv_tbl.default <- function(x) {
  message("Chris, you need to work more on this package.")
  x
}

#' Coerce objet to an advanced edge list
#'
#' Advanced edge lists only contain information about the edges and include:
#' \itemize{
#'   \item source_nodeset_class
#'   \item source_nodeset_name
#'   \item sourcce_node_name
#'   \item target_nodeset_class
#'   \item target_nodeset_name
#'   \item target_node_name
#'   \item link_value
#'   \item network_name
#' }
#'
#' @param x object to coerce to advanced edge list
#' @param ... aditional arguments to pass on to methods
#'
#' @return
#' @export
#'
#' @examples
as_adv_edge <- function(x, ...) {
  UseMethod("as_adv_edge")
}

#' @export
#' @rdname as_adv_edge
as_adv_edge.adv_tbl <- function(x) {
  x %>%
    select(-contains("source"), -contains("target"),
           source_node_name, target_node_name) %>%
    select(source_node_name, target_node_name, everything()) %>%
    set_adv_tbl_edge_class()
}

#' @param source_node_name column with the source node name
#' @param target_node_name column with the target node name
#'
#' @export
#' @rdname as_adv_edge
as_adv_edge.data.frame <- function(x, source_node_name, target_node_name) {
  # check for missing args
  missing_arg_handler()

  source_node_name <- enquo(source_node_name)
  target_node_name <- enquo(target_node_name)
  x %>%
    rename(source_node_name = !!source_node_name,
           target_node_name = !!target_node_name) %>%
    set_adv_tbl_edge_class()
}
#' Coerce objet to an advanced attribute list
#'
#' Advanced attribute lists only contain information about the nodes and
#' include:
#' \itemize{
#'   \item nodeset_class
#'   \item nodeset_name
#'   \item node_name
#'   }
#' Additionally, there are three columns for every attribute - a column with the
#' name of the attribute, a column with the type that the attribute is, and a
#' column with the attribute value
#'
#' @param x object to coerce to advanced edge list
#' @param ... aditional arguments to pass on to methods
#'
#' @return
#' @export
#'
#' @examples
as_adv_attr <- function(x, ...) {
  UseMethod("as_adv_attr")
}

#' @export
#' @rdname as_adv_attr
as_adv_attr.adv_tbl <- function(x) {
  source_attr <-
    x %>%
    select(contains("source")) %>%
    gather(key = "key", value = "value",
           -source_node_name, -source_nodeset_class, -source_nodeset_name)
  if("key" %in% colnames(source_attr)) {
    source_attr <-
      source_attr %>%
      mutate(key = stringr::str_remove(key, "^source_")) %>%
      separate(key, into = c("attr_name", "col"), sep = "_attr_") %>%
      filter(col !="name") %>%
      spread(col, value) %>%
      rename(attr_type = type, attr_value = value)
  }
  source_attr <-
    source_attr %>%
    select(nodeset_class = source_nodeset_class,
           nodeset_name = source_nodeset_name,
           node_name = source_node_name,
           starts_with("attr")) %>%
    distinct()

  target_attr <- x %>%
    select(contains("target")) %>%
    gather(key = "key", value = "value",
           -target_node_name, -target_nodeset_class, -target_nodeset_name)
  if("key" %in% colnames(target_attr)) {
    target_attr <-
      target_attr %>%
      mutate(key = stringr::str_remove(key, "^target_")) %>%
      separate(key, into = c("attr_name", "col"), sep = "_attr_") %>%
      filter(col !="name") %>%
      spread(col, value) %>%
      rename(attr_type = type, attr_value = value)
  }
  target_attr <-
    target_attr %>%
    select(nodeset_class = target_nodeset_class,
           nodeset_name = target_nodeset_name,
           node_name = target_node_name,
           starts_with("attr")) %>%
    distinct()

  bind_rows(source_attr, target_attr) %>%
    distinct() %>%
    set_adv_tbl_attr_class()
}

#' @param nodeset_class column with the nodeset class
#' @param nodeset_name column with the nodeset name
#' @param node_name column with the node names
#'
#' @export
#' @rdname as_adv_attr
as_adv_attr.data.frame <- function(x, nodeset_class, nodeset_name, node_name,
                                   group_col) {
  # check for missing args
  missing_arg_handler()

  nodeset_class <- enquo(nodeset_class)
  nodeset_name <- enquo(nodeset_name)
  node_name <- enquo(node_name)
  group_col <- enquo(group_col)
  group_col_name <- quo_name(group_col)

  x <- x %>%
    rename(nodeset_class = !!nodeset_class,
           nodeset_name = !!nodeset_name,
           node_name = !!node_name)
  attr_types <-
    x %>%
    summarise_all(class) %>%
    tidyr::gather(attr_name, attr_type)
  x %>%
    tidyr::gather(key = attr_name, value = attr_value,
                  -nodeset_class, -nodeset_name, -node_name,
                  -group_col_name) %>%
    left_join(attr_types, by = "attr_name") %>%
    select(nodeset_class, nodeset_name, node_name,
           attr_name, attr_type, attr_value) %>%
    set_adv_tbl_attr_class()
}
