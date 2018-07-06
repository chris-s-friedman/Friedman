
as_adv_tbl <- function(x, ...) {
  UseMethod("as_adv_table")
}

# Methods:
## igraph
## edge list
## edge list and a node list
## data frame

as_adv_tbl.default <- function(x) {
  message("Chris, you need to work more on this package.")
  x
}

as_adv_edge <- function(x, source_node_name, target_node_name) {
  # check for missing args
  missing_arg_handler()

  source_node_name <- enquo(source_node_name)
  target_node_name <- enquo(target_node_name)
  x %>%
    rename(source_node_name = !!source_node_name,
           target_node_name = !!target_node_name) %>%
    set_adv_tbl_edge_class()
}
as_adv_attr <- function(x, nodeset_class, nodeset_name, node_name) {
  # check for missing args
  missing_arg_handler()

  nodeset_class <- enquo(nodeset_class)
  nodeset_name <- enquo(nodeset_name)
  node_name <- enquo(node_name)

  x <-
    x %>% rename(nodeset_class = !!nodeset_class,
                 nodeset_name = !!nodeset_name,
                 node_name = !!node_name)

  attr_types <-
    x %>%
    summarise_all(class) %>%
    tidyr::gather(attr_name, attr_type)

  x %>%
    tidyr::gather(key = attr_name, value = attr_value,
                  -nodeset_class, -nodeset_name, -node_name) %>%
    left_join(attr_types, by = "attr_name") %>%
    select(nodeset_class, nodeset_name, node_name,
           attr_name, attr_type, attr_value) %>%
    set_adv_tbl_attr_class()
}
