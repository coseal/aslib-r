#' Returns algorithm names of scenario.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{character}].
#' @export
getAlgorithmNames = function(asscenario) {
  names(asscenario$desc$metainfo_algorithms)
}

