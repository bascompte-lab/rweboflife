test_that("connectance returns known values", {
  library(igraph)
  library(dplyr)

  path_to_file <- "../testdata/selected_NWs.RData"
  load(path_to_file)

  nw_list <-list("M_PL_015","M_PL_044","M_PL_054","M_PL_056")

  conn_vec <- c()
  for (my_nw_name in nw_list) {

    my_nw <- filter(selected_nws, network_name == my_nw_name)

    my_graph <- my_nw %>% select(species1, species2, connection_strength) %>%
      graph_from_data_frame()
    my_inc_mat <- incidence_matrix_from_graph(my_graph)

    conn <- connectance(my_nw) %>% round(., 5)

    conn_vec <- append(conn_vec, conn)

  }

  expected_vec <- c(0.03362, 0.01679, 0.02151, 0.02622)

  expect_equal(conn_vec, expected_vec)
})
