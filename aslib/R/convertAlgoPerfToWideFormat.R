#' Converts \code{algo.runs} object of a scenario to wide format.
#'
#' The first 2 columns are \dQuote{instance_id} and \dQuote{repetition}. The remaining ones are
#' the measured performance values.
#' The feature columns are in the same order as \dQuote{features_deterministic},
#' \dQuote{features_stochastic} in the description object.
#' code{NA} means the performance value is not available, possibly because the algorithm run was aborted.
#' The data.frame is sorted by \dQuote{instance_id}, then \dQuote{repetition}.
#'
#' @param desc [\code{\link{ASScenarioDesc}}]\cr
#'   Description object of scenario.
#' @param algo.runs [\code{data.frame}]\cr
#'   Algo runs data.frame from scenario.
#' @param measure [\code{character(1)}]\cr
#'   Selected performance measure.
#'   Default is first measure in scenario.
#' @return [\code{data.frame}].
#' @export
convertAlgoPerfToWideFormat = function(desc, algo.runs, measure) {
  assertClass(desc, "ASScenarioDesc")
  assertClass(algo.runs, "data.frame")
  measure = checkMeasure(measure, desc)

  ap = dcast(algo.runs, instance_id + repetition ~ algorithm, value.var = measure)
  # sort rows and cols
  ap = ap[, c("instance_id", "repetition",
    names(desc$metainfo_algorithms))]
  sortByCol(ap, c("instance_id", "repetition"))
}
