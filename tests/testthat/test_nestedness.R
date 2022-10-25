test_that("the nestedness of a perfecly nested matrix is 1 and of a rnd matrix is < 0.3", {

  perfect_nested <- function(my_rows, my_cols){
    mat <- matrix(0, my_rows, my_cols)
    for (i in seq(1,my_rows,1)) {
      j_max <- ceiling(i*my_cols/my_rows)
      for (j in seq(1,j_max,1)) {
        mat[i,j] <-1
      }
    }
    return(mat)
  }

  n <- 30 # number of rows
  m <- 40 # number of columns

  nested_mat <- perfect_nested(n,m)
  expect_equal(1, nestednessR(nested_mat)) # R version
  expect_equal(1, nestedness(nested_mat))  # cpp version

  # random matrix
  M <- matrix(0,n,m)
  for(i in 1:n){
    for (j in 1:m){
      M[i,j] <- floor(runif(1, min = 0, max = 1.2)) # binary random sparse
    }
  }

  expect_true(nestednessR(M) < 0.3) # R version
  expect_true(nestedness(M) < 0.3) # cpp version

})

