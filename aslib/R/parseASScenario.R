#' @title Parses the data files of an algorithm selection scenario into an S3 object.
#'
#' @description
#'
#' Object members
#'
#' Let n be the number of (replicated) instances, m the number of unique instances,
#' p the number of features, s the number of feature steps and k the number of algorithms.
#
#  @details
#' \describe{
#' \item{desc [\code{\link{ASScenarioDesc}}]}{Description object, containing further info.}
#' \item{feature.runstatus [\code{data.frame(n, s + 2)}]}{Runstatus of feature computation steps.
#'   The first 2 columns are \dQuote{instance_id} and \dQuote{repetition}, the remaining are the status factors.
#'   The step columns are in the same order as the feature steps in the description object.
#'   The factor levels are always: ok, presolved, crash, timeout, memout, other.
#'   No entry can be \code{NA}.
#'   The data.frame is sorted by \dQuote{instance_id}, then \dQuote{repetition}.}
#' \item{feature.costs [\code{data.frame(n, s + 2)}]}{Costs of feature computation steps.
#'   The first 2 columns are \dQuote{instance_id} and \dQuote{repetition}, the remaining are
#'   numeric costs of the feature steps.
#'   The step columns are in the same order as the feature steps in the description object.
#'   code{NA} means the cost is not available, possibly because the feature computation was aborted.
#'   The data.frame is sorted by \dQuote{instance_id}, then \dQuote{repetition}.
#'   If no cost file is available at all, \code{NULL} is stored.}
#' \item{feature.values [\code{data.frame(n, p + 2)}]}{Measured feature values of instances.
#'   The first 2 columns are \dQuote{instance_id} and \dQuote{repetition}. The remaining ones are
#'   the measured instance features.
#'   The feature columns are in the same order as \dQuote{features_deterministic},
#'   \dQuote{features_stochastic} in the description object.
#'   code{NA} means the feature is not available, possibly because the feature computation was aborted.
#'   The data.frame is sorted by \dQuote{instance_id}, then \dQuote{repetition}.}
#' \item{algo.runs [\code{data.frame}]}{Runstatus and performance information of the
#'   algorithms. Simply the parsed ARFF file.
#'   See \code{\link{convertAlgoPerfToWideFormat}} for a more convenient format.}
#' \item{algo.runstatus [\code{data.frame(n, k + 2)}]}{Runstatus of algorithm runs.
#'   The first 2 columns are \dQuote{instance_id} and \dQuote{repetition}, the remaining are the status factors.
#'   The step columns are in the same order as the feature steps in the description object.
#'   The factor levels are always: ok, presolved, crash, timeout, memout, other.
#'   No entry can be \code{NA}.
#'   The data.frame is sorted by \dQuote{instance_id}, then \dQuote{repetition}.}
#' \item{cv.splits[\code{data.frame(m, 3)}]}{Definition of cross-validation splits for each replication
#'   of a repeated CV with folds.
#'   Has columns \dQuote{instance_id}, \dQuote{repetition} and \dQuote{fold}.
#'   The instances with fold = i for a replication r constitute the i-th test set for the r-th CV.
#'   The training set is the \dQuote{instance_id} column with repetition = r, in the same order,
#'   when the test set is removed.
#'   The data.frame is sorted by \dQuote{repetition}, then \dQuote{fold}, then \dQuote{instance_id}.
#'   If no CV file is available at all, \code{NULL} is stored, and a warning is issued, although this
#'   should not happen.}
#' }
#'
#' @param path [\code{character(1)}]\cr
#'   Path to directory of benchmark data set.
#' @return [\code{\link{ASScenario}}]. Description object.
#' @examples
#' \dontrun{
#'   sc = parseASScenario("/path/to/scenario")
#' }
#' @export
#' @seealso \code{\link{writeASScenario}}
#' @aliases ASScenario
parseASScenario = function(path) {
  assertDirectory(path, access = "r")

  desc = parseDescription(path)
  fsteps = names(desc$feature_steps)

  ### build feature.runstatus
  feature.runstatus = readARFF(file.path(path, "feature_runstatus.arff"))
  colnames(feature.runstatus) = make.names(colnames(feature.runstatus))
  statusLevels = c("ok", "timeout", "memout", "presolved", "crash", "other", "unknown")
  # make sure we have correct levels
  for (j in 3:ncol(feature.runstatus)) {
    factors = factor(feature.runstatus[, j])
    if(!setequal(union(factors, statusLevels), statusLevels)) {
        stop(paste("Feature runstatus file contains illegal levels:", setdiff(factors, statusLevels)))
    }
    feature.runstatus[, j] = factor(feature.runstatus[, j],
      levels = statusLevels)
  }
  # sort rows and cols
  feature.runstatus = feature.runstatus[, c("instance_id", "repetition", fsteps)]
  feature.runstatus = sortByCol(feature.runstatus, c("instance_id", "repetition"))

  ### build feature.costs
  costfile = file.path(path, "feature_costs.arff")
  if (file.exists(costfile)) {
    feature.costs = readARFF(costfile)
    colnames(feature.costs) = make.names(colnames(feature.costs))
    # sort rows and cols
    feature.costs = feature.costs[, c("instance_id", "repetition", fsteps)]
    feature.costs = sortByCol(feature.costs, c("instance_id", "repetition"))
  } else {
    feature.costs = NULL
  }

  ### build feature.values
  feature.values = readARFF(file.path(path, "feature_values.arff"))
  colnames(feature.values) = make.names(colnames(feature.values))
  # sort rows and cols
  feature.values = feature.values[, c("instance_id", "repetition",
    desc$features_deterministic, desc$features_stochastic)]
  feature.values = sortByCol(feature.values, c("instance_id", "repetition"))

  algo.runs = readARFF(file.path(path, "algorithm_runs.arff"))
  colnames(algo.runs) = make.names(colnames(algo.runs))
  algo.runs$algorithm = make.names(algo.runs$algorithm)
  algo.runs = sortByCol(algo.runs, c("instance_id", "repetition"))

  ### build algo.runstatus
  algo.runstatus = dcast(algo.runs, instance_id + repetition ~ algorithm, value.var = "runstatus")
  # sort rows and cols
  algo.runstatus = algo.runstatus[, c("instance_id", "repetition",
    names(desc$metainfo_algorithms))]
  algo.runstatus = sortByCol(algo.runstatus, c("instance_id", "repetition"))

  ### build cv.splits
  cv.file = file.path(path, "cv.arff")
  if (file.exists(cv.file)) {
    cv.splits = readARFF(cv.file)
    colnames(cv.splits) = make.names(colnames(cv.splits))
    instancesInCV = length(unique(cv.splits$instance_id))
    instancesInAlgos = length(unique(algo.runstatus$instance_id))
    if(instancesInCV != instancesInAlgos) {
        stop(paste("Fold allocations given for ", instancesInCV, "instances, but algorithms run on", instancesInAlgos, "instances!"))
    }
    # sort rows and cols
    cv.splits = cv.splits[, c("instance_id", "repetition", "fold")]
    cv.splits = sortByCol(cv.splits, c("repetition", "fold", "instance_id"))
  } else {
    warningf("No cv file exists for scenario at:\n%s", path)
    cv.splits = NULL
  }

  makeS3Obj("ASScenario",
    desc = desc,
    feature.runstatus = feature.runstatus,
    feature.costs= feature.costs,
    feature.values = feature.values,
    algo.runs = algo.runs,
    algo.runstatus = algo.runstatus,
    cv.splits = cv.splits
  )
}

#' @export
print.ASScenario = function(x, ...) {
  d = x$desc
  printField1 = function(name, val) {
    k = length(val)
    if (k == 0L)
      catf("%-30s        : -", name)
    else if (k == 1L)
      catf("%-30s        : %s", name, clipString(as.character(val), 60L))
    else
      catf("%-30s (%3i)  : %s", name, k, clipString(collapse(val, sep = ", "), 60L))
  }
  x$feature_steps = names(x$feature_steps)
  printField1("Scenario id", d$scenario_id)
  printField1("Performance measures", d$performance_measures)
  printField1("Performance types", d$performance_type)
  printField1("Algorithm cutoff time", d$algorithm_cutoff_time)
  printField1("Algorithm cutoff mem", d$algorithm_cutoff_memory)
  printField1("Feature cutoff time", d$features_cutoff_time)
  printField1("Feature cutoff mem", d$features_cutoff_memory)
  printField1("Nr. of instances", length(unique(x$feature.values$instance_id)))
  printField1("Features (deterministic)", d$features_deterministic)
  printField1("Features (stochastic)", d$features_stochastic)
  printField1("Feature repetitions", collapse(range(x$feature.values$repetition), sep = " - "))
  printField1("Feature costs", ifelse(is.null(x$feature.costs), "No", "Yes"))
  printField1("Algo.", names(d$metainfo_algorithms))
  printField1("Algo. repetitions", collapse(range(x$algo.runs$repetition), sep = " - "))
  printField1("Algo. runs (inst x algo x rep)", nrow(x$algo.runs))
  printField1("Feature steps", names(d$feature_steps))
  printField1("CV repetitions", ifelse(is.null(x$cv.splits), "No", getNumberOfCVReps(x)))
  printField1("CV folds", ifelse(is.null(x$cv.splits), "No", getNumberOfCVFolds(x)))
}

