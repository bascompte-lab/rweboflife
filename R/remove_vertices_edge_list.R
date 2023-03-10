#' @NoRd
#' @import igraph
largest_cluster_size <- function(graph_input){
  max(components(graph_input)$csize)
}

#' @NoRd
measure_graph <- function(N_in, my_edge_list){

  if(dim(my_edge_list)[1]==0){
    #print("last pair")
    size_el <- 1
    row <- c(N_in, 1., 1., 0., 0., 0.)
  } else {

    v1 <- unique(my_edge_list[,1])
    v2 <- unique(my_edge_list[,2])
    n_nodes1 <- length(v1)
    n_nodes2 <- length(v2)
    n_nodes <- length(union(v1,v2))
    my_graph <- graph_from_edgelist(my_edge_list, directed = FALSE)
    C_max <- largest_cluster_size(my_graph)
    if(!is.finite(C_max))C_max <- 0.
    row <- c(N_in, n_nodes, n_nodes1, n_nodes2, mean(degree(my_graph)), sd(degree(my_graph)), C_max)

  }
  row
}

#' @NoRd
removal_sequence <- function(edge_list, my_vec_rm, desc_degree, rnd_same_deg = TRUE)
{
  # Careful: nodes without links are overlooked in the edge list

  vec_n1 <- edge_list[,1]
  vec_n2 <- edge_list[,2]

  freq_n1 <- table(vec_n1) %>% as.data.frame()
  freq_n2 <- table(vec_n2) %>% as.data.frame()

  df1 <- freq_n1 %>% full_join(.,freq_n2, by=c("vec_n1"="vec_n2"))
  df_node_deg <- df1 %>% replace(is.na(.), 0) %>%
    mutate(node = vec_n1, deg = Freq.x + Freq.y) %>%
    select(node, deg)

  df_node_deg$node <- as.numeric(as.character(df_node_deg$node))
  df_rm <- filter(df_node_deg, node %in% my_vec_rm)

  k_list <- distinct(df_rm, deg)$deg %>% sort(decreasing = desc_degree)

  vec_rm_by_deg <- c()
  for (k in k_list) {
    node_tmp <- filter(df_rm, deg == k)$node
    if (length(node_tmp) > 1 & rnd_same_deg) {
      node_tmp <- sample(node_tmp)
    }
    vec_rm_by_deg <- append(vec_rm_by_deg, node_tmp)
  }
  vec_rm_by_deg
}


#' @export
inverse_percolation <-function(my_edge_list, my_vec_rm, iter_max, imeasure, my_seed){
  # https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/

  v1 <- unique(my_edge_list[,1])
  v2 <- unique(my_edge_list[,2])
  n_nodes1 <- length(v1)
  n_nodes2 <- length(v2)
  n_nodes_in <- length(union(v1,v2))

  set.seed(my_seed)           # set seed for rnd removal


  df_tmp <- NULL

  for(iter in seq(1,iter_max,1)){

    cat("\n")
    print("---------------------------------")
    print(paste0("RND removal strategy, iter = ",iter))

    vec_rm_rnd <- sample(my_vec_rm)

    edge_list_old <- my_edge_list
    index <- 0

    for (rm in vec_rm_rnd){

      if(index%%imeasure ==0) {
        row <- append(index,measure_graph(n_nodes_in, edge_list_old))
        # print(row)
        df_tmp <- rbind(df_tmp, row)
      }

      index <- index + 1

      if(dim(edge_list_old)[1]!=0){

        log_n1 <- edge_list_old[,1] != rm
        log_n2 <- edge_list_old[,2] != rm
        log_rm <- log_n1 & log_n2

        num_nodes <- dim(edge_list_old)[1]
        edge_list_new <- edge_list_old[log_rm,]
        edge_list_old <- edge_list_new

      }

      if (!(is.matrix(edge_list_old)) || dim(edge_list_old)[1]==0){
        break
      }
    }
  }

  df_tmp <- df_tmp %>% as.data.frame()
  colnames(df_tmp) <- c("id","N","n_nodes", "n_nodes1", "n_nodes2", "k_ave", "k_sd", "C_max") 
  rownames(df_tmp) <- NULL

  df_tmp %>%
    group_by(id) %>%
    summarise_all(mean)

}


#' @export
systematic_removal <-function(my_edge_list, my_vec_rm, iter_max, imeasure, desc_degree, rnd_same_deg = TRUE, my_seed)
{
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/

  v1 <- unique(my_edge_list[,1])
  v2 <- unique(my_edge_list[,2])
  n_nodes1 <- length(v1)
  n_nodes2 <- length(v2)
  n_nodes_in <- length(union(v1,v2))

  set.seed(my_seed)           # set seed for rnd removal

  df_tmp <- NULL

  for(iter in seq(1,iter_max,1)){

    cat("\n")
    print("---------------------------------")
    print(paste0("Systematic removal desc_degree ",desc_degree," iter = ",iter))


    edge_list_old <- my_edge_list
    index <- 0

    vec_rm_rnd <- my_vec_rm

    while (length(vec_rm_rnd) > 0) {

      vec_rm_rnd <- removal_sequence(edge_list_old, vec_rm, desc_degree, rnd_same_deg)
      rm <- head(vec_rm_rnd,1)

      if(index%%imeasure == 0) {
        row <- append(index,measure_graph(n_nodes_in, edge_list_old))
        # print(row)
        df_tmp <- rbind(df_tmp, row)
      }

      index <- index + 1

      if(dim(edge_list_old)[1]!=0){

        log_n1 <- edge_list_old[,1] != rm
        log_n2 <- edge_list_old[,2] != rm
        log_rm <- log_n1 & log_n2

        num_nodes <- dim(edge_list_old)[1]
        edge_list_new <- edge_list_old[log_rm,]
        edge_list_old <- edge_list_new

      }

      if (dim(edge_list_old)[1]==0 || !(is.matrix(edge_list_old))){
        break
      }
    }
  }

  df_tmp <- df_tmp %>% as.data.frame()
  colnames(df_tmp) <- c("id","N","n_nodes", "n_nodes1", "n_nodes2", "k_ave", "k_sd", "C_max") 
  rownames(df_tmp) <- NULL

  df_tmp %>%
    group_by(id) %>%
    summarise_all(mean)
}


