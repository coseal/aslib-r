library(knitr)
library(ggplot2)
library(xtable)
library(devtools)
library(aslib)
library(BBmisc)
library(stringr)
source("defs.R")
source("eda_config.R")
source("shortenPathsInValidator.R")
source("convertResultsToXTable.R")

data.dir =  normalizePath(coseal.data.dir)
scenario.dirs = list.dirs(data.dir, recursive = FALSE)
# avoid bbob and machine learning for now
scenario.dirs = scenario.dirs[!str_detect(scenario.dirs, ".git")]
scenario.dirs = scenario.dirs[!str_detect(scenario.dirs, "TSP-")]
# scenario.dirs = scenario.dirs[1]
rhtml.dir = normalizePath("../Rhtml")
html.dir = normalizePath("../html")
config.dir = normalizePath("../configs")

old.wd = getwd()

if (interactive()) {
  url.prefix =  sprintf("file://%s", html.dir)
} else {
  url.prefix = "http://coseal.github.io/aslib-r/scenario-pages"
}

ee = new.env()
ee$data.dir = data.dir
ee$scenario.dirs = scenario.dirs
ee$url.prefix = url.prefix
res = load2("llama_results.RData")
res = res$res
sel = load2("selection_results.RData")
ids = unname(unlist(lapply(sel, function(d) d$id)))
selected = lapply(sel, function(d)
  list(
    selected.feats = d$all.feats[d$feats$x$x == 1],
    selected.algos = d$all.solvers[d$solvs$x$x == 1],
    feat.perf = unname(d$feats$y),
    algo.perf = unname(d$solvs$y),
    n.all.feats = length(d$all.feats),
    n.selected.feats = sum(d$feats$x$x),
    n.all.algos = length(d$all.solvers),
    n.selected.algos = sum(d$solvs$x$x)
  ))
names(selected) = ids
ee$asscenarios = list()


try({

  for (scenario.dir in scenario.dirs) {
    setwd(old.wd)
    scenario.name = basename(scenario.dir)
    messagef("Create pages for: %s", scenario.name)

    # set scenario data for knitr
    asscenario = parseASScenario(scenario.dir)
    ee$asscenarios[[asscenario$desc$scenario_id]] = asscenario
    ee$scenario.dir = scenario.dir
    ee$asscenario = asscenario
    ee$selection.results = selected[[scenario.name]]
    ee$llama.results = subset(res, prob == scenario.name)

    # create output dir
    out.dir = file.path(html.dir, scenario.name)
    unlink(out.dir, recursive = TRUE)
    dir.create(out.dir)
    setwd(out.dir)

    # set global options for chunks
    opts_chunk$set(
      echo = FALSE,
      # fig.path = file.path(out.dir, "figure/"),
      fig.width = 10,
      fig.height = 10
    )

    # set global ggplot options
    theme_set(theme_gray(base_size = 18))

    # read EDA HTML config for scenario
    config = readEDAConfig(asscenario, config.dir)
    ee$config = config

    # helper to knit rhtml files
    knitIt = function(file, out = file) {
      knit(
        file.path(rhtml.dir, paste(file, "Rhtml", sep = ".")),
        output = file.path(out.dir, paste(out, "html", sep = ".")),
        env = ee, quiet = TRUE
      )
    }

    # create all subpages and copy readme
    knitIt("index")
    knitIt("scenario_index", "index")
    file.copy(file.path(scenario.dir, "readme.txt"), out.dir)
    knitIt("data_files")
    knitIt("algos")
    knitIt("features")
    knitIt("llama")
    knitIt("config")
    knitIt("validator")
  }

  knit(file.path(rhtml.dir, "index.Rhtml"), output = file.path(html.dir, "index.html"),
    env = ee, quiet = TRUE)

})


setwd(old.wd)
