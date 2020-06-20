#' @title Creates a registry which can be used for running several Llama models on a cluster.
#'
#' @description
#' It is likely that you need to install some additional R packages for this from CRAN or extra
#' Weka learner. The latter can be done via e.g. \code{WPM("install-package", "XMeans")}.
#'
#' Feature costs are added for real prognostic models but not for baseline models.
#'
#' @param asscenarios [(list of) \code{\link{ASScenario}}]\cr
#'   Algorithm selection scenarios.
#' @param feature.steps.list [\code{list} of \code{character}]\cr
#'   Named list of feature steps we want to use.
#'   Must be named with scenario ids.
#'   Default is to take the default feature steps from the scenario.
#' @param baselines [\code{character}]\cr
#'   Vector of characters, defining the baseline models.
#'   Default is c("vbs", "singleBest", "singleBestByPar", "singleBestBySuccesses").
#' @param learners [list of \code{\link[mlr]{Learner}}]\cr
#'   mlr learners to use for modeling.
#'   Default is none.
#' @param par.sets [list of \code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Param sets for learners to tune via random search.
#'   Pass an empty param set, if you want no tuning.
#'   Must be in of same length as \code{learners} and in the same order.
#'   Default is none.
#' @param rs.iters [\code{integer(1)}]\cr
#'   Number of iterations for random search hyperparameter tuning.
#'   Default is 100.
#' @param n.inner.folds [\code{integer(1)}]\cr
#'   Number of cross-validation folds for inner CV in hyperparameter tuning.
#'   Default is 2L.
#' @return BatchExperiments registry.
#' @export
runLlamaModels = function(asscenarios, feature.steps.list = NULL, baselines = NULL,
  learners = list(), par.sets = list(), rs.iters = 100L, n.inner.folds = 2L) {

  asscenarios = ensureVector(asscenarios, 1L, cl = "ASScenario")
  assertList(asscenarios, types = "ASScenario")
  scenario.ids = extractSubList(asscenarios, c("desc", "scenario_id"), use.names = FALSE)
  names(asscenarios) = scenario.ids

  if (is.null(feature.steps.list)) {
    feature.steps.list = extractSubList(asscenarios, c("desc", "default_steps"),
      simplify = FALSE, use.names = TRUE)
  } else {
    feature.steps.list = ensureVector(feature.steps.list, 1L, cl = "character")
    assertList(feature.steps.list, types = "character", names = "unique")
    assertSetEqual(names(feature.steps.list), scenario.ids, ordered = FALSE)
  }
  # sort in correct order
  feature.steps.list = feature.steps.list[scenario.ids]

  # models and defaults
  baselines.all = c("vbs", "singleBest", "singleBestByPar", "singleBestBySuccesses")

  # check model args
  if (is.null(baselines)) {
    baselines = baselines.all
  } else {
    assertCharacter(baselines, any.missing = FALSE)
    assertSubset(baselines, baselines.all)
  }

  assertList(learners, types = "Learner")
  learner.ids = extractSubList(learners, "id")
  assertList(par.sets, types = "ParamSet", len = length(learners))

  rs.iters = asInt(rs.iters, lower = 1L)
  n.inner.folds = asInt(n.inner.folds, lower = 2L)

  packs = c("RWeka", "llama", "methods", "ParamHelpers", "mlr", "BatchExperiments")
  requirePackages(packs, why = "runLlamaModels")

  llama.scenarios = mapply(convertToLlama, asscenario = asscenarios,
    feature.steps = feature.steps.list, SIMPLIFY = FALSE)
  llama.cvs = lapply(asscenarios, convertToLlamaCVFolds)

  # FIXME:
  unlink("run_llama_models-files", recursive = TRUE)
  reg = makeExperimentRegistry("run_llama_models", packages = packs)
  batchExport(reg, fixFeckingPresolve = fixFeckingPresolve, doNestedCVWithTuning = doNestedCVWithTuning,
    tuneLlamaModel = tuneLlamaModel)

  for (i in seq_along(asscenarios)) {
    asscenario = asscenarios[[i]]
    desc = asscenario$desc
    cutoff = desc$algorithm_cutoff_time
    timeout = if (desc$performance_type[[1L]] == "runtime" && !is.na(cutoff)) {
      cutoff
    } else {
      NULL
    }
    addProblem(reg, id = desc$scenario_id,
      static = list(
        asscenario = asscenario,
        feature.steps = feature.steps.list[[desc$scenario_id]],
        timeout = timeout,
        llama.scenario = llama.scenarios[[i]],
        llama.cv = llama.cvs[[i]],
        n.algos = length(getAlgorithmNames(asscenario)),
        rs.iters = rs.iters,
        n.inner.folds = n.inner.folds,
        makeRes = function(data, p, timeout, addCosts) {
          if (addCosts) {
            data = fixFeckingPresolve(asscenario, data)
          }
          list(
            predictions = p$predictions,
            succ = mean(successes(data, p, timeout = timeout, addCosts = addCosts)),
            par10 = mean(parscores(data, p, timeout = timeout, addCosts = addCosts)),
            mcp = mean(misclassificationPenalties(data, p))
          )
        }
      )
    )
  }

  # add baselines to reg
  if (length(baselines) > 0L) {
    addAlgorithm(reg, id = "baseline", fun = function(static, type) {
      llama.fun = get(type, envir = asNamespace("llama"))
      p = llama.fun(data = static$llama.scenario)
      p = list(predictions = p)
      # this is how LLAMA checks what type of argument is given to the evaluation function
      attr(p, "hasPredictions") = TRUE
      static$makeRes(static$llama.scenario, p, static$timeout, FALSE)
    })
    des = makeDesign("baseline", exhaustive = list(type = baselines))
    addExperiments(reg, algo.designs = des)
  }

  # add real selectors
  addLearnerAlgoAndExps = function(lrn, par.set) {
    # BE does not like the dots in mlr ids
    id = str_replace_all(lrn$id, "\\.", "_")
    addAlgorithm(reg, id = id, fun = function(static) {
      llama.fun = switch(lrn$type,
        classif = llama::classify,
        regr = llama::regression,
        cluster = llama::cluster
      )
      if (lrn$type == "cluster") {
        pre = llama::normalize
      } else {
        pre = function(x, y = NULL) {
          list(features = x)
        }
      }
      p = if (ParamHelpers::isEmpty(par.set))
        llama.fun(lrn, data = static$llama.cv, pre = pre)
      else
        doNestedCVWithTuning(static$asscenario, static$llama.cv, pre, static$timeout,
          lrn, par.set, llama.fun, static$rs.iters, static$n.inner.folds)
      static$makeRes(static$llama.cv, p, static$timeout, TRUE)
    })
    addExperiments(reg, algo.designs = id)
  }

  if (length(learners) > 0L)
    mapply(addLearnerAlgoAndExps, learners, par.sets)

  return(reg)
}

doNestedCVWithTuning = function(asscenario, ldf, pre, timeout, learner, par.set, llama.fun,
  rs.iters, n.inner.folds) {

  n.outer.folds = length(ldf$test)
  outer.preds = vector("list", n.outer.folds)

  for (i in 1:n.outer.folds) {
    ldf2 = ldf
    ldf2$data = ldf$data[ldf$train[[i]],]
    ldf2$train = NULL
    ldf2$test = NULL
    ldf3 = cvFolds(ldf2, nfolds = n.inner.folds, stratify = FALSE)
    parvals = tuneLlamaModel(asscenario, ldf3, pre, timeout, learner, par.set, llama.fun, rs.iters)

    # now fit only on outer trainining set with best params and predict outer test set
    learner2 = setHyperPars(learner, par.vals = parvals)
    outer.split.ldf = ldf
    outer.split.ldf$train = list(ldf$train[[i]])
    outer.split.ldf$test = list(ldf$test[[i]])
    outer.preds[[i]] = llama.fun(learner2, data = outer.split.ldf, pre = pre)
  }
  retval = outer.preds[[1]]
  retval$predictions = do.call(rbind, lapply(outer.preds, function(x) { x$predictions }))
  return(retval)
}

tuneLlamaModel = function(asscenario, cv.splits, pre, timeout, learner, par.set, llama.fun, rs.iters) {
  des = ParamHelpers::generateRandomDesign(rs.iters, par.set, trafo = TRUE)
  des.list = ParamHelpers::dfRowsToList(des, par.set)
  requirePackages(c("parallelMap"), why = "tuneLlamaModel")
  parallelStartMulticore()
  ys = parallelMap(function(x) {
    par10 = try({
      learner = setHyperPars(learner, par.vals = x)
      p = llama.fun(learner, data = cv.splits, pre = pre)
      ldf = fixFeckingPresolve(asscenario, cv.splits)
      par10 = mean(parscores(ldf, p, timeout = timeout))
      messagef("[Tune]: %s : par10 = %g", ParamHelpers::paramValueToString(par.set, x), par10)
      return(par10)
    })
    if(inherits(par10, "try-error")) {
      par10 = NA
    }
    return(par10)
  }, des.list, simplify = TRUE)
  parallelStop()
  # messagef"[Tune]: Tuning evals failed: %i", sum(is.na(ys))]
  best.i = getMinIndex(ys)
  best.parvals = des.list[[best.i]]
  messagef("[Best]: %s : par10 = %g", ParamHelpers::paramValueToString(par.set, best.parvals), ys[best.i])
  return(best.parvals)
}




