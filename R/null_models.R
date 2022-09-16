#' @NoRd
pbc <-function(r, k, kring){
  kneigh <- (r+k) %% kring
  if(kneigh == 0) kneigh <- kring
  return(kneigh)
}

#' @NoRd
row_col_from_index <- function(index, nc){
  j <- index %% nc
  if(j == 0) j <- nc
  i <- ceiling(index/nc)
  return(c(i,j))
}

#' @NoRd
index_from_row_col <- function(i, j, nc){
  index <- nc*(i-1) + j
  return(index)
}

#' @NoRd
create_indexes <- function(nr, nc) {
  # create a vector with randomly selected indexes defining a 2x2 sub-matrix of M
  # select the first index at rnd
  index1 <- ceiling(runif(1, min = 0, max = nr*nc))
  pair <- row_col_from_index(index1, nc)
  i1 <- pair[1]
  j1 <- pair[2]
  # move along column j1 by rr
  rr <- ceiling(runif(1, min = 0, max = nr-1))
  i2 <- pbc(rr, i1, nr)
  index2 <- index_from_row_col(i2, j1, nc) # 2nd index
  # move along row i1 by rr
  rr <- ceiling(runif(1, min = 0, max = nc-1))
  j2 <- pbc(rr, j1, nc)
  index3 <- index_from_row_col(i1, j2, nc) # 3rd index
  index4 <- index_from_row_col(i2, j2, nc) # 4th index

  return(c(index1,index2,index3,index4))
}

#' @NoRd
count_nonzero <- function(M){
  MM <- as.matrix((M>0))
  class(MM) <- "numeric"
  return(sum(MM))
}


#' @NoRd
equifrequent_model <- function(M_in){

  rows <- nrow(M_in)
  columns <- ncol(M_in)
  number_ones <- count_nonzero(M_in)
  count_ones <- 0

  M_equif <- matrix(0, rows, columns)

  while (count_ones < number_ones) {

    x <- ceiling(runif(1, min = 0, max = rows))
    y <- ceiling(runif(1, min = 0, max = columns))

    while (M_equif[x,y] == 1) {

      x <- ceiling(runif(1, min = 0, max = rows))
      y <- ceiling(runif(1, min = 0, max = columns))

    }

    M_equif[x,y] <- 1
    count_ones <- count_ones + 1;
  }

  return(M_equif)
}


#' @NoRd
cell_model <- function(M_in){

  rows <- nrow(M_in)
  columns <- ncol(M_in)

  # binarize M_in
  M <- as.matrix((M_in>0))
  class(M) <- "numeric"

  PR <- matrix(0, rows, 1)
  PC <- matrix(0, columns, 1)
  M_cell <- matrix(0, rows, columns)

  for (i in 1:rows){
    number_ones <- 0
    for (j in 1:columns){
      if(M[i,j] == 1){
        number_ones <- number_ones + 1
      }
    }
    PR[i] <- number_ones/columns
  }

  for (j in 1:columns){
    number_ones <- 0
    for (i in 1:rows){
      if(M[i,j] == 1){
        number_ones <- number_ones + 1
      }
    }
    PC[j] <- number_ones/rows
  }

  for (i in 1:rows){
    for (j in 1:columns){
      p <- (PR[i]+PC[j])/2;
      r <- runif(1)
      if( r < p ){
        M_cell[i,j] <- 1;
      }
    }

  }

  return(M_cell)
}


#' @NoRd
#' @import permute
swap_model = function(M_in, iter_max){

  nr <- nrow(M_in) # number of rows
  nc <- ncol(M_in) # number of columns

  # initialize M (randomized matrix) iter loop
  M <- M_in

  for(iter in 1:iter_max){

    # binarize M into a logical matrix
    B <- as.matrix((M>0))

    # flatten logical matrix with row-major order
    M_vec <- as.vector(t(B))

    allEqual <- TRUE
    while (allEqual == TRUE){
      indexes <- create_indexes(nr, nc)
      sub_vec <- M_vec[indexes]
      allEqual <- xor(all(sub_vec), all(!(sub_vec)))
    }

    # shaffle indexes till all positions are swapped
    fullRND <- FALSE
    while (fullRND == FALSE){
      indexes_rnd <- indexes[shuffle(indexes)]
      fullRND <- all(indexes_rnd != indexes)
    }

    M_vec[indexes_rnd] <- sub_vec
    M_swap <- matrix(M_vec, nrow=nr, ncol=nc, byrow=TRUE) # back to matrix

    M <- M_swap
    class(M_swap) <- "numeric"
  }

  return(M_swap)
}


#' @export
null_model <- function(M_in, iter_max = ceiling(nrow(M_in)*ncol(M_in)*2/3), model = NULL){
  # Make sure we are working with a matrix
  M_in <- as.matrix(M_in)
  if(is.null(model)){
    stop("No type argument assigned for the null model. Please choose one from the list: \"swap\", \"equifrequent\", \"cell\"")
  } else {
    if (model == "swap"){
      M_out <- swap_model(M_in, iter_max)
    } else if (model == "equifrequent") {
      M_out <- equifrequent_model(M_in)
    }  else if (model == "cell") {
      M_out <- cell_model(M_in)
    }
  }
  return(M_out)
}
