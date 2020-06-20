#' @rdname plotAlgoPerf
#' @export
plotAlgoPerfScatterMatrix = function(asscenario, measure, impute.zero.vals = FALSE,
  log = FALSE, rm.censored.runs = TRUE) {

  if (rm.censored.runs)
    asscenario = removeCensoredRuns(asscenario)
  z = getEDAAlgoPerf(asscenario, measure, impute.failed.runs = TRUE, jitter = TRUE,
    impute.zero.vals = impute.zero.vals, check.log = log, format = "wide", with.instance.id = FALSE)
  perf.range = range(as.numeric(unlist(z$data)))
  pairs(z$data, log = ifelse(log, "xy", ""), xlim = perf.range, ylim = perf.range)
}
