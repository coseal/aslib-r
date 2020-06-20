#' S3 class for ASScenarioDesc.
#'
#' Object members
#'
#' \describe{
#' \item{scenario_id [\code{character(1)}]}{Name of scenario.}
#' \item{performance_measures [\code{character}]}{Names of measures.}
#' \item{maximize [named \code{character}]}{Maximize measure?}
#' \item{performance_type [named \code{character}]}{Either \dQuote{runtime} or
#'   \dQuote{solution_quality}.}
#' \item{algorithm_cutoff_time [\code{numeric(1)}]}{Cutoff time for an algorithm run.}
#' \item{algorithm_cutoff_memory [\code{numeric(1)}]}{Cutoff memory for an algorithm run.}
#' \item{features_cutoff_time [\code{numeric(1)}]}{Cutoff time for a feature run.}
#' \item{features_cutoff_memory [\code{numeric(1)}]}{Cutoff memory for a feature run.}
#' \item{feature_steps [named \code{list} of \code{character}]}{Names of feature processing steps, the other feature steps they require, and the features they provide.}
#' \item{metainfo_algorithms [named \code{list} of lists of \code{character}]}{Names of algorithms and meta-information about them.}
#' }
#' @name ASScenarioDesc
#' @rdname ASScenarioDesc
NULL

# Parses description file and returns an S3 class of the contents
parseDescription = function(path) {
  assertDirectory(path, access = "r")

  # do not warn about EOL
  desc = yaml.load_file(file.path(path, "description.txt"))

  assertSubset(c("scenario_id", "performance_measures", "maximize", "performance_type", "algorithm_cutoff_time", "algorithm_cutoff_memory", "features_cutoff_time", "features_cutoff_memory", "features_deterministic", "features_stochastic", "number_of_feature_steps", "default_steps", "feature_steps", "metainfo_algorithms"), names(desc))

  # now handle all non-scalar strings and convert them to proper data types
  convertField = function(name, cast = as.character) {
    val = desc[[name]]
    if (length(val) == 0L || all(val == "")) {
      val = character(0)
    } else if (length(val) == 1 && val == "?") {
      val = NA
    }
    desc[[name]] <<- cast(val)
  }

  convertField("performance_measures", make.names)
  convertField("performance_type", make.names)
  convertField("maximize", as.logical)
  convertField("algorithm_cutoff_time", as.numeric)
  convertField("algorithm_cutoff_memory", as.numeric)
  convertField("features_cutoff_time", as.numeric)
  convertField("features_cutoff_memory", as.numeric)
  convertField("features_deterministic", make.names)
  convertField("features_stochastic", make.names)
  convertField("default_steps", make.names)
  convertField("number_of_feature_steps", as.numeric)

  names(desc$feature_steps) = make.names(names(desc$feature_steps))
  desc$feature_steps = lapply(desc$feature_steps, function(fs) {
    fs$provides = make.names(fs$provides)
    if (!is.null(fs$requires)) fs$requires = make.names(fs$requires)
    return(fs)
  })

  names(desc$metainfo_algorithms) = make.names(names(desc$metainfo_algorithms))

  desc$maximize = setNames(desc$maximize, desc$performance_measures)
  desc$performance_type = setNames(desc$performance_type, desc$performance_measures)

  addClasses(desc, "ASScenarioDesc")
}

