# functions for adv_table data

adv_table <- function(attributes, edges, directed = TRUE) {
  if(!"adv_tbl_attr" %in% class(attributes)) {
    stop("coerce attributes to adv_tbl_attr with as_adv_attr()")
  }
  if(!"adv_tbl_edge" %in% class(edges)) {
    stop("coerce edges to adv_tbl_edge with as_adv_edge()")
  }
  attributes <- attributes %>%
    mutate(col_name_safe = attr_name) %>%
    gather(variable, value, attr_type, contains("attrs")) %>%
    unite(colname, col_name_safe, variable) %>%
    spread(colname, value)
  source_attr <-
    attributes %>%
    rename_all(funs(paste0("source_", .)))
  targ_attr <-
    attributes %>%
    rename_all(funs(paste0("target_", .)))
  edges %>%
    left_join(source_attr, by = "source_node_name") %>%
    left_join(targ_attr, by = "target_node_name") %>%
    set_adv_tbl_class() %>%
    select(source_nodeset_class, source_nodeset_name, source_node_name,
           target_nodeset_class, target_nodeset_name, target_node_name,
           everything())
}

is_adv_table <- function(x) {
  "adv_tbl" %in% class(x)
}



