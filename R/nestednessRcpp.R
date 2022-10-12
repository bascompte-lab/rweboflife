#' @export
#' @rawNamespace useDynLib(weboflife)
nestednessCpp <- function(M) {
  .Call('_weboflife_nestednessCpp', PACKAGE = 'weboflife', M)
}

