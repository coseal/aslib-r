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
#' \item{feature.runstatus [\code{data.frame(n, s + 2)}]}{Runstatus of instance feature computation steps.
#'   The first 2 columns are \dQuote{instance_id} and \dQuote{repetition}, the remaining are the status factors.
#'   The step columns are in the same order as the feature steps in the description object.
#'   The factor levels are always: ok, presolved, crash, timeout, memout, other.
#'   No entry can be \code{NA}.
#'   The data.frame is sorted by \dQuote{instance_id}, then \dQuote{repetition}.}
#' \item{algorithm.feature.runstatus [\code{data.frame(k, s + 1)}]}{Runstatus of algorithm feature computation steps.
#'   The first column is \dQuote{algorithm}, the remaining are the status factors.
#'   The step columns are in the same order as the feature steps in the description object.
#'   The factor levels are always: ok, crash, timeout, memout, other.
#'   No entry can be \code{NA}.
#'   The data.frame is sorted by \dQuote{algorithm}.}
#' \item{feature.costs [\code{data.frame(n, s + 2)}]}{Costs of instance feature computation steps.
#'   The first 2 columns are \dQuote{instance_id} and \dQuote{repetition}, the remaining are
#'   numeric costs of the instance feature steps.
#'   The step columns are in the same order as the feature steps in the description object.
#'   \code{NA} means the cost is not available, possibly because the feature computation was aborted.
#'   The data.frame is sorted by \dQuote{instance_id}, then \dQuote{repetition}.
#'   If no cost file is available at all, \code{NULL} is stored.}
#' \item{algorithm.feature.costs [\code{data.frame(n, s + 1)}]}{Costs of algorithm feature computation steps.
#'   The first column is \dQuote{algorithm}, the remaining are
#'   numeric costs of the algorithmic feature steps.
#'   The step columns are in the same order as the feature steps in the description object.
#'   \code{NA} means the cost is not available, possibly because the feature computation was aborted.
#'   The data.frame is sorted by \dQuote{algorithm}.
#'   If no cost file is available at all, \code{NULL} is stored.}
#' \item{feature.values [\code{data.frame(n, p + 2)}]}{Measured feature values of instances.
#'   The first 2 columns are \dQuote{instance_id} and \dQuote{repetition}. The remaining ones are
#'   the measured instance features.
#'   The feature columns are in the same order as \dQuote{instance_features_deterministic},
#'   \dQuote{features_stochastic} in the description object.
#'   \code{NA} means the feature is not available, possibly because the feature computation was aborted.
#'   The data.frame is sorted by \dQuote{instance_id}, then \dQuote{repetition}.}
#' \item{algorithm.feature.values [\code{data.frame(k, p + 1)}]}{Measured feature values of algorithms
#'   The first column is \dQuote{algorithm}. The remaining ones are
#'   the measured algorithmic features.
#'   The feature columns are in the same order as \dQuote{algorithm_features_deterministic},
#'   \dQuote{algorithm_features_stochastic} in the description object.
#'   \code{NA} means the feature is not available, possibly because the feature computation was aborted.
#'   The data.frame is sorted by \dQuote{algorithm}.}
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
  algo_fsteps = names(desc$algorithm_feature_steps)

  ### build feature.runstatus
  feature.runstatus = readRunstatus(path = path, filename = "feature_runstatus.arff", 
                statusLevels = c("ok", "timeout", "memout", "presolved", "crash", "other", "unknown"), 
                fsteps = fsteps, sortBy = c("instance_id", "repetition"))
  
  ### build algorithm.feature.runstatus
  algorithm.feature.runstatus = readRunstatus(path = path, filename = "algorithm_feature_runstatus.arff", 
                statusLevels = c("ok", "timeout", "memout", "crash", "other", "unknown"), 
                fsteps = algo_fsteps, sortBy = c("algorithm", "repetition"), ignoreExists = TRUE)
  if (!is.null(algorithm.feature.runstatus)) {
    algorithm.feature.runstatus$algorithm = make.names(algorithm.feature.runstatus$algorithm)
  }

  ### build feature.costs
  costfile = file.path(path, "feature_costs.arff")
  feature.costs = readCosts(filename = costfile, 
                                     fsteps = fsteps, sortBy = c("instance_id", "repetition"))
  
  algorithm.costfile = file.path(path, "algorithm_feature_costs.arff")
  algorithm.feature.costs = readCosts(filename = algorithm.costfile, 
                                     fsteps = fsteps, sortBy = c("algorithm", "repetition"))
  if (!is.null(algorithm.feature.costs)) {
    algorithm.feature.costs$algorithm = make.names(algorithm.feature.costs$algorithm)
  }

  ### build feature.values
  feature.values = readFeatureValues(path = path, 
              filename = "feature_values.arff",
              sortBy = c("instance_id", "repetition"), 
              featureSort = c(desc$features_deterministic, desc$features_stochastic), 
              ignoreExists = FALSE)
  
  ### build algorithm.feature.values
  algorithm.feature.values = readFeatureValues(path = path, 
              filename = "algorithm_feature_values.arff",
              sortBy = c("algorithm", "repetition"), 
              featureSort = c(desc$algorithm_features_deterministic, desc$algorithm_features_stochastic), 
              ignoreExists = TRUE)
  if (!is.null(algorithm.feature.values)) {
    algorithm.feature.values$algorithm = make.names(algorithm.feature.values$algorithm)
  }

  algo.runs = read.arff(file.path(path, "algorithm_runs.arff"))
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
    cv.splits = read.arff(cv.file)
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
            feature.costs = feature.costs,
            feature.values = feature.values,
            algorithm.feature.runstatus = algorithm.feature.runstatus,
            algorithm.feature.costs = algorithm.feature.costs,
            algorithm.feature.values = algorithm.feature.values,
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
  printField1("Instance Feature cutoff time", d$features_cutoff_time)
  printField1("Instance Feature cutoff mem", d$features_cutoff_memory)
  printField1("Algorithm Feature cutoff time", d$algorithm_features_cutoff_time)
  printField1("Algorithm Feature cutoff mem", d$algorithm_features_cutoff_memory)
  printField1("Nr. of instances", length(unique(x$feature.values$instance_id)))
  printField1("Instance Features (deterministic)", d$features_deterministic)
  printField1("Instance Features (stochastic)", d$features_stochastic)
  printField1("Algorithm Features (deterministic)", d$algorithm_features_deterministic)
  printField1("Algorithm Features (stochastic)", d$algorithm_features_stochastic)
  printField1("Feature repetitions", collapse(range(x$feature.values$repetition), sep = " - "))
  printField1("Feature costs", ifelse(is.null(x$feature.costs), "No", "Yes"))
  printField1("Algo.", names(d$metainfo_algorithms))
  printField1("Algo. repetitions", collapse(range(x$algo.runs$repetition), sep = " - "))
  printField1("Algo. runs (inst x algo x rep)", nrow(x$algo.runs))
  printField1("Feature steps", names(d$feature_steps))
  printField1("CV repetitions", ifelse(is.null(x$cv.splits), "No", getNumberOfCVReps(x)))
  printField1("CV folds", ifelse(is.null(x$cv.splits), "No", getNumberOfCVFolds(x)))
}

# Helper function for reading feature runstatus files
readRunstatus = function(path, filename, statusLevels, fsteps, sortBy, ignoreExists = FALSE) {
  res = tryCatch({
      feature.runstatus = read.arff(file.path(path, filename))
      colnames(feature.runstatus) = make.names(colnames(feature.runstatus))
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
      feature.runstatus = feature.runstatus[, c(sortBy, fsteps)]
      feature.runstatus = sortByCol(feature.runstatus, sortBy)
    }, error = function(e) {
      if (ignoreExists) {
        return(NULL)
      } else {
        stop(e)
      }
    }
  )
  
  return(res)
}

# Helper function for reading feature cost files
readCosts = function(filename, fsteps, sortBy) {
  if (file.exists(filename)) {
    feature.costs = read.arff(filename)
    colnames(feature.costs) = make.names(colnames(feature.costs))
    # sort rows and cols
    feature.costs = feature.costs[, c(sortBy, fsteps)]
    feature.costs = sortByCol(feature.costs, sortBy)
  } else {
    feature.costs = NULL
  }
  
  return(feature.costs)
}

# Helper function for reading feature value files
readFeatureValues = function(path, filename, sortBy, featureSort, ignoreExists = FALSE) {
  res = tryCatch({
    feature.values = read.arff(file.path(path, filename))
    colnames(feature.values) = make.names(colnames(feature.values))
    # sort rows and cols
    feature.values = feature.values[, c(sortBy, featureSort)]
    feature.values = sortByCol(feature.values, sortBy)
  }, error = function(e) {
      if (ignoreExists) {
        return(NULL)
      } else {
        stop(e)
      }
    }
  )
  
  return(res)
}
