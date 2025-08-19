#' Creates summary data.table for runLlamaModel experiments.
#'
#' @param reg [\code{\link[batchtools]{Registry}}]\cr
#'   batchtools registry.
#' @param ids [\code{data.table}]\cr
#'   Selected job ids. Default is all submitted jobs.
#' @param fun [\code{function()}]\cr
#'   Function to aggregate results with. Default is a function
#'   that returns succ, par10 and mcp values.
#'   For a detailed description, see [\link[batchtools]{reduceResultsList}].
#' @param missing.val [\code{list(1)}]\cr
#'   List with defaults for missing values that are needed for
#'   aggregation.
#'   For a detailed description, see [\link[batchtools]{reduceResultsList}].
#' @return [\code{data.table}].
#' @export
summarizeLlamaExps = function(reg, ids = findSubmitted(),
                    fun = function(job, res) {
                    return(list(succ = res$succ, par10 = res$par10, mcp = res$mcp))
                    }, missing.val = list(succ = 0, par10 = Inf, mcp = Inf)) {
  assertRegistry(reg)
  assertDataTable(ids)
  assertFunction(fun)
  assertList(missing.val)

  info = unwrap(getJobPars(reg = reg))
  repls = getJobTable(reg = reg, ids = ids)
  repls = repls[, c("job.id", "repl")]
  d = reduceResultsDataTable(reg = reg, ids = findSubmitted(), fun = fun,
                             missing.val = missing.val)

  d = merge(info, d, by = "job.id")
  d = merge(repls, d, by = "job.id")

  # rename columns so they match columns from old results
  colnames(d) = c("id", "repl", "prob", "algo", "type", "result")
  d = d[, c(1, 3, 4, 5, 2, 6)]

  # get column names
  ns = names(d$result[[1]])
  for(i in seq_along(1:length(ns))) {
    d[[ns[i]]] = NA
  }

  # unlist result
  for(i in seq_along(1:nrow(d))) {
    for(j in seq_along(1:length(ns))) {
      d[[i, ns[j]]] = d[i, ]$result[[1]][[j]]
    }
  }
  d$result = NULL

  return(d)
}
