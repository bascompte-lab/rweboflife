#' @export
#' @import igraph rjson jsonlite curl
incidence_matrix_from_graph <- function(my_graph){

  # Add the "type" attribute to the vertices of the graph
  V(my_graph)$type <- bipartite_mapping(my_graph)$type

  my_inc_mat <- as_incidence_matrix(
    my_graph,
    attr = "connection_strength",
    names = TRUE,
    sparse = FALSE
  )

  # convert elements into numeric values
  class(my_inc_mat) <-"numeric"

  # remove NA values (previous empty strings)
  my_inc_mat[which(is.na(my_inc_mat) == T)]<-0

  return(my_inc_mat)
}
