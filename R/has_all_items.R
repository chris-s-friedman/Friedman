#' Does a df grouping column have all possible instances?
#'
#' check that a data frame has all of the specified items in the specified
#' column. if it does not have those items, add them. Also has functionality to
#' alter the added rows.
#'
#' @param df data frame to check
#' @param column column that should have all of the items specified in col_items
#' @param col_items items that should be present in col_items
#' @param cols_to_modify named list of columns that should be modified, where
#'   list names correspond to column names and values correspond to new values
#'   that should be in the added rows
#'
#' @return a df
#'
#' @import dplyr purrr tidyr
#' @export
#'
#' @examples
#' df <- data.frame(groups = letters[1:5], value = rnorm(5))
#' has_all_items(df, groups, letters[1:10], list(value = 0))
#'
has_all_items <- function(df, column, col_items, cols_to_modify = NULL) {
  column <- enquo(column)
  if(any(!col_items %in% (df %>%
                          select_at(vars(!!column)) %>%
                          unlist() %>% unique()))){
    needed_items <- col_items[!col_items %in% (df %>%
                                                 select_at(vars(!!column)) %>%
                                                 unlist() %>% unique())] %>%
      paste(., collapse = ",")
    missing_df <- df %>%
      select_at(vars(-!!column))
    if(nrow(missing_df) == 0) {
      missing_df <- missing_df %>%
        add_row
    }
    missing_df <- missing_df %>%
      mutate(!!(quo_name(column)) := needed_items) %>%
      separate_rows(!!column, sep = ",")
    if(!is.null(cols_to_modify)){
      cols_to_keep <- colnames(missing_df)[!colnames(missing_df) %in%
                                             names(cols_to_modify)]
      missing_df <- map2_dfc(names(cols_to_modify),
                             cols_to_modify, function(name, value){
                               missing_df %>%
                                 select_at(vars(!!name)) %>%
                                 mutate(!!name := value)
                             }) %>%
        bind_cols(missing_df %>%
                    select_at(cols_to_keep))
    }
    df <- bind_rows(df, missing_df %>% distinct())
  }
  df
}
