set_edge_list_class <- function(x) {
  class(x) <- c("edge_list", "tbl_df", "tbl", "data.frame")
  x
}
set_attribute_list_class <- function(x) {
  class(x) <- c("attr_list", "tbl_df", "tbl", "data.frame")
  x
}
set_adv_tbl_class <- function(x) {
  class(x) <- c("adv_tbl", "tbl_df", "tbl", "data.frame")
  x
}
set_adv_tbl_edge_class <- function(x) {
  class(x) <- c("adv_tbl_edge", "tbl_df", "tbl", "data.frame")
  x
}
set_adv_tbl_attr_class <- function(x) {
  class(x) <- c("adv_tbl_attr", "tbl_df", "tbl", "data.frame")
  x
}
