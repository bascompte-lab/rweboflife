#' @export
#' @import fastmatch permute
remove_rows <- function(bipartite_network, n_iter, strategy, i_seed) {

  set.seed(i_seed)

  if (strategy == "RND") print(paste0("RND strtegy"))
  if (strategy == "MTL") print(paste0("MTL strtegy"))
  if (strategy == "LTM") print(paste0("LTM strtegy"))

  print(paste0("dim ", dim(bipartite_network)))

  v_rows <- rownames(bipartite_network)

  res <- c()

  rows_ext <- replicate(length(v_rows),0)
  columns_ext <- replicate(length(v_rows),0)

  for (iter in 1:n_iter){

    print(paste0("iter = ",iter))

    iprim_ext <- 0
    isec_ext <- 0
    iloop <- 0
    df_old <- bipartite_network

    if (strategy == "RND") v_rnd <- v_rows[shuffle(v_rows)]
    if (strategy == "LTM") v_rnd <- order_and_shuffle_bipartite(bipartite_network, FALSE)[,1]
    if (strategy == "MTL") v_rnd <- order_and_shuffle_bipartite(bipartite_network, TRUE)[,1]

    for (row in v_rnd){

      # print(row)

      iloop <- iloop + 1 # counter

      ip <- which(rownames(df_old) == row)

      if (length(ip) != 0) df <- df_old[-ip,]

      iprim_ext <- iprim_ext + length(ip)

      if(is.null(dim(df))) {
#        print("achtung dim(df) is null and iterations stop!!!")
        rows_ext[iloop] <- rows_ext[iloop] + nrow(bipartite_network)
        columns_ext[iloop] <- columns_ext[iloop] + ncol(bipartite_network)
        break
      }

      if(nrow(df) == 0){
        rows_ext[iloop] <- rows_ext[iloop] + nrow(bipartite_network)
        columns_ext[iloop] <- columns_ext[iloop] + ncol(bipartite_network)
        break
      }

      #
      #  # check secondary extinction
      v_extinction <- c()
      v_columns <- colnames(df)

      for(col in v_columns){
        w <- df[,col]
        if (all(w == 0)) {
          # print(paste0(col, "  goes extinct"))
          v_extinction <- c(v_extinction, col)
          isec_ext <- isec_ext + 1
        }
      }

      if(length(v_extinction)>0){

        for (aa in v_extinction){
          ja <- fmatch(aa, colnames(df))
          df <- df[ ,-ja]

        }
      }

      rows_ext[iloop] <- rows_ext[iloop] + iprim_ext
      columns_ext[iloop] <- columns_ext[iloop] + isec_ext

      df_old <- df

    }
  }


  res <- cbind(rows_ext/n_iter,columns_ext/n_iter)

  res_data <- data.frame(res)
  names(res_data) <-c("removed_rows","removed_columns")
  return(res_data)
}


#' @NoRd
order_and_shuffle_bipartite <- function(bipartite_network,desc){

  # Compute degree for Row Species and order in descending if desc == TRUE
  degree_vector <- sort(rowSums(bipartite_network),decreasing = desc)
  # Extract the Species names associated to the ordering
  names_vector <- names(degree_vector)
  # Convert names and degree to data frame
  name_degree_df <- data.frame(names_vector,unname(degree_vector))
  # Name the columns of the dataframe
  names(name_degree_df) <- c("name","degree")
  # Initialize empty dataframe to store shuffling
  shuffled_df <- data.frame(NULL)

  # Loop through unique degree values
  for (i in unique(name_degree_df$degree)) {
    # Find all Species names with i degree and store in vector
    name_by_degree <- name_degree_df$name[which(name_degree_df$degree == i)]
    # Shuffle vector
    shf_name_by_degree <- sample(name_by_degree)
    # Create a new dataframe for the shuffled names and current degree value
    shf_df_by_degree <- data.frame(as.matrix(shf_name_by_degree,nrow=1),(as.matrix(rep(i,length(shf_name_by_degree)),nrow = 1)))
    # Name the current degree dataframe
    names(shf_df_by_degree) <- c("name","degree")
    # Fill the overall dataframe with current shuffle
    shuffled_df <- rbind(shuffled_df,shf_df_by_degree)
  }
  # Return overall dataframe
  return(shuffled_df)
}
