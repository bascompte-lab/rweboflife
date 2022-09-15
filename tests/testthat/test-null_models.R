test_that("check that the number of 1 values is preserved in the swap null model", {

  n <- 25 # number of rows
  m <- 22 # number of columns
  M <- matrix(0,n,m)

  for(i in 1:n){
    for (j in 1:m){
      M[i,j] <- floor(runif(1, min = 0, max = 1.5)) # binary matrix
    }
  }

  M_swap <- null_model(M, iter_max = 20, model = "swap")
  expect_equal(sum(M_swap), sum(M))
})


test_that("check that the overlapping between a sparse input matrix and the output matrix lies within a range", {

  n <- 37 # number of rows
  m <- 32 # number of columns
  M <- matrix(0,n,m)

  for(i in 1:n){
    for (j in 1:m){
      M[i,j] <- floor(runif(1, min = 0, max = 1.18)) # binary sparse matrix
    }
  }

  ones_fraction <- sum(M)/(nrow(M)*ncol(M))
  min_overlap <- 1-2*ones_fraction
  max_overlap <- 1-ones_fraction

  M_swap <- null_model(M, model = "swap") # iter_max = n*m by default

  overlap_mat <- (M_swap == M)
  class(overlap_mat) <- "numeric"
  overlap <- sum(overlap_mat)/(nrow(M)*ncol(M))

  expect_true((overlap < max_overlap) && (overlap > min_overlap))
})

