#' Convert advanced format data into lists of edges and vertices
#'
#' Converts advanced format network data (a la ORA) into an igraph object
#'
#' @param advanced_format_data data in advanced file format. Should be a
#'   data.frame.it Should have the following columns, at the least: \itemize{
#'   \item "Source nodeset name" \item "Source node name" \item "Target nodeset
#'   name" \item "Target node name" \item "Frequency" \item "Network name" \item
#'   "CHILD_ID" but if the complete advanced file is supplied, the other columns
#'   will not be used.}
#' @param sourcenodesetname unqouted column name
#' @param sourcenodename unqouted column name
#' @param targetnodesetname unqouted column name
#' @param targetnodename unqouted column name
#' @param network unqouted column name
#' @param frequency unqouted column name
#' @param directed Logical scalar, whether or not to create a directed graph.
#'   (passed to \code{igraph::graph_from_data_frame()})
#'
#' @return an igraph graph object
#'
#' @author Chris Friedman, \email{chris.s.friedman@@gmail.com}
#' @seealso
#'
#' @examples graph_from_adv_data(advanced_format_data)
#'
#' @import  dplyr
#' @importFrom igraph graph_from_data_frame
#'
#' @export
#'
graph_from_adv_data <- function(advanced_format_data,
                                sourcenodesetname, sourcenodename,
                                targetnodesetname, targetnodename,
                                network, frequency, directed = TRUE){
  sourcenodesetname <- enquo(sourcenodesetname)
  sourcenodename <- enquo(sourcenodename)
  targetnodesetname <-  enquo(targetnodesetname)
  targetnodename <- enquo(targetnodename)
  network <- enquo(network)
  frequency <- enquo(frequency)

  # create the edge list
  edge <- advanced_format_data %>%
    select(!!sourcenodename, !!targetnodename, !!network, !!frequency) %>%
    rename(from_id = !!sourcenodename,
           to_id = !!targetnodename)
  # create the vertex list
  vert_source <- advanced_format_data %>%
    select(!!sourcenodename, !!sourcenodesetname) %>%
    rename(label = !!sourcenodename,
           nodeset = !!sourcenodesetname) %>%
    distinct()
  vert_targ <- advanced_format_data %>%
    select(!!targetnodename, !!targetnodesetname) %>%
    rename(label = !!targetnodename,
           nodeset = !!targetnodesetname) %>%
    distinct()
  # There is an issue where during the join, the vert dfs keep the var,
  # "CHILDID". This fixes that.
  attr(vert_source, "vars") <- NULL
  attr(vert_targ, "vars") <- NULL
  # join the vertex lists together
  vert <- full_join(vert_source, vert_targ, by = c("label", "nodeset"))
  # make the igraph object
  graph_from_data_frame(d = edge,
                        directed = directed,
                        vertices = vert)
  }
