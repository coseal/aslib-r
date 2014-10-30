library(BBmisc)
library(devtools)
library(llama)
library(stringr)
library(BatchExperiments)
library(checkmate)
load_all("../algselbench")
source("defs.R")

source("searchSequential.R")
source("searchSequentialObjective.R")

ds.dirs = list.files(file.path(coseal.svn.dir, "data"), full.names = TRUE)
ds.dirs = ds.dirs[!str_detect(ds.dirs, "BBOB|MACHINE")]
print(ds.dirs)
#ds.dirs = ds.dirs[1]
asscenarios = lapply(ds.dirs, parseASScenario)

file.dir <- tempfile("selection_exps_")
reg = makeRegistry("run_selection_exps", file.dir = file.dir, packages = c("llama", "mlr", "BatchExperiments", "parallelMap"), seed = 123)

ctrl = makeSSControl(method = "sbs")

batchMap(reg, fun = function(ast) {
  ldf = convertToLlamaCVFolds(ast)
  n.bits = length(getFeatureNames(ast))
  feats = searchSequential(searchSequentialObjectiveFeatures, n.bits, control = ctrl, ldf = ldf,
    llama.model.fun = regression, mlr.learner = makeLearner("regr.randomForest"))
  n.bits = length(getAlgorithmNames(ast))
  solvs = searchSequential(searchSequentialObjectiveSolvers, n.bits, control = ctrl, ldf = ldf,
    llama.model.fun = regression, mlr.learner = makeLearner("regr.randomForest"))

  cbind(data.frame(id = ast$desc$scenario_id, perfFeats = feats$y, perfSolvers = solvs$y),
      do.call(cbind, setNames(as.list(feats$x[[1]]), getFeatureNames(ast))),
      do.call(cbind, setNames(as.list(solvs$x[[1]]), getAlgorithmNames(ast))))
}, asscenarios)

submitJobs(reg, resources = list(memory = 2048))
waitForJobs(reg)

res = reduceResults(reg, fun = function(aggr, job, res) {
    rbind(aggr, res)
}, init = data.frame())
save2(file = "selection_results.RData", res = res)