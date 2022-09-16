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
#' @import permute
swap_model = function(M_in, iter_max){

  M <- M_in
  nr <- nrow(M) # number of rows
  nc <- ncol(M) # number of columns

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
null_model <- function(M_in, iter_max = ncol(M_in)*nrow(M_in), model = NULL){
  # Make sure we are working with a matrix
  M_in <- as.matrix(M_in)
  if(is.null(model)){
    stop("No type argument assigned for the null model. Please choose one from the list: \"swap\", \"equifrequent\", \"cell\"")
  } else {
    if (model == "swap"){
      M_out <- swap_model(M_in, iter_max)
    } else if (model == "equifrequent") {
      M_out <- M_in
      stop("Sorry, the equifrequent null model has not been implemented yet")
    }  else if (model == "cell") {
      M_out <- M_in
      stop("Sorry, the cell null model has not been implemented yet")
    }
  }
  return(M_out)
}
