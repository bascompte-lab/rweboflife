#' @export
trim_network = function(network){
  # Removes any columns or rows without interactions
  network = network[,colSums(network != 0) > 0]
  network = network[rowSums(network !=0) > 0,]
  return(network)
}
