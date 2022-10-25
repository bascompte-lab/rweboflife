#' @export
nestednessR <- function(M){

  # this code computes the nestedness of a given incident matrix M
  # according to the definition given in
  # Fortuna, M.A., et al.: Coevolutionary dynamics shape the structure of bacteria‐phage infection networks. Evolution 1001-1011 (2019).
  # DOI 10.1111/evo.13731

  # Make sure we are working with a matrix
  M <- as.matrix(M)
  # Binarize the matrix
  B <- as.matrix((M>0))
  class(B) <- "numeric"

  # Get number of rows and columns
  nrows <- nrow(B)
  ncols <- ncol(B)

  # Compute nestedness of rows
  nestedness_rows <- 0
  for(i in 1:(nrows-1)){
    for(j in (i+1): nrows){

      c_ij <- sum(B[i,] * B[j,])      # Number of interactions shared by i and j
      k_i <- sum(B[i,])               # Degree of node i
      k_j <- sum(B[j,])               # Degree of node j

      if (k_i == 0 || k_j==0) {next}  # Handle case if a node is disconnected

      o_ij <- c_ij / min(k_i, k_j)    # Overlap between i and j

      nestedness_rows <- nestedness_rows + o_ij
    }
  }

  # Compute nestedness of columns
  nestedness_cols <- 0
  for(i in 1: (ncols-1)){
    for(j in (i+1): ncols){

      c_ij <- sum(B[,i] * B[,j])      # Number of interactions shared by i and j
      k_i <- sum(B[,i])               # Degree of node i
      k_j <- sum(B[,j])               # Degree of node j
      if (k_i == 0 || k_j==0) {next}  # Handle case if a node is disconnected.

      o_ij <- c_ij / min(k_i, k_j)    # Overlap between i and j

      nestedness_cols <- nestedness_cols + o_ij
    }
  }

  # Compute nestedness of the network
  nestedness_val <- (nestedness_rows + nestedness_cols) / ((nrows * (nrows - 1) / 2) + (ncols * (ncols - 1) / 2))

  return(nestedness_val)
}

