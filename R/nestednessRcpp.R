#' @export
#' @rawNamespace useDynLib(weboflife)
nestedness <- function(M) {
  .Call('_weboflife_nestednessCpp', PACKAGE = 'weboflife', M)
}

