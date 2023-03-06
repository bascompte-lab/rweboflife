#' @export
#' @rawNamespace useDynLib(rweboflife)
nestedness <- function(M) {
  .Call('_rweboflife_nestednessCpp', PACKAGE = 'rweboflife', M)
}

