test_that("check number of columns and their names", {

  library(rjson)
  library(igraph)
  library(dplyr)

  base_url <- "https://www.web-of-life.es/"

  my_nw_name <- "M_PL_056"
  json_url <- paste0(base_url,"get_networks.php?network_name=",my_nw_name)
  my_nw <- jsonlite::fromJSON(json_url)

  # select the 3 relevant columns and create the igraph object
  my_graph <- my_nw %>% select(species1, species2, connection_strength) %>%
    graph_from_data_frame()

  my_inc_mat <- incidence_matrix_from_graph(my_graph)

  iterations <- 5
  removal_strategy <- "RND" # "MTL" # "LTM"
  df <- remove_rows(my_inc_mat, iterations, removal_strategy, 645) %>%
    mutate("network_name" = my_nw_name, "strategy" = removal_strategy)

  expect_length(colnames(df), 4)

  expect_equal(colnames(df), c("removed_rows","removed_columns","network_name","strategy"))

})

test_that("number of removed columns equals the rows of the incidence matrix", {


  base_url <- "https://www.web-of-life.es/"

  nw_list <-list("M_PL_015","M_PL_044","M_PL_054","M_PL_056")

  rows_removed_vec <- c()
  expected_vec <- c()

  for (my_nw_name in nw_list) {
    json_url <- paste0(base_url,"get_networks.php?network_name=",my_nw_name)
    my_nw <- jsonlite::fromJSON(json_url)

    my_graph <- my_nw %>% select(species1, species2, connection_strength) %>%
      graph_from_data_frame()
    my_inc_mat <- incidence_matrix_from_graph(my_graph)

    iterations <- 5
    removal_strategy <- "RND" # "MTL" # "LTM"
    df <- remove_rows(my_inc_mat, iterations, removal_strategy, 145) %>%
      mutate("network_name" = my_nw_name, "strategy" = removal_strategy)

    rows_removed <- tail(df,2)[1,]$removed_rows

    rows_removed_vec <- append(rows_removed_vec, rows_removed)
    expected_vec <- append(expected_vec, nrow(my_inc_mat))
  }

  expect_equal(rows_removed_vec, expected_vec)

})
