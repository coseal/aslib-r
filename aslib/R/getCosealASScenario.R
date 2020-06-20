#' @title Retrieves a scenario from the Coseal Github repository and parses into an S3 object.
#'
#' @description
#'
#' Uses subversion export to retrieve a specific scenario from the official
#' Coseal Github repository. The scenario is checked out into a temporary directory
#' and parsed with \code{parseASScenario}.
#'
#' @param name [\code{character(1)}]\cr
#'   Name of benchmark data set.
#' @return [\code{\link{ASScenario}}]. Description object.
#' @examples
#' \dontrun{
#'   sc = getCosealASScenario("CSP-2010")
#' }
#' @export
getCosealASScenario = function(name) {
    res = system2("svn", c("help", "2>&1"), stdout = "/dev/null")
    if(res == 127) stop("Need subversion commandline client!")

    path = file.path(tempdir(), name)
    dir.create(path, showWarnings = FALSE)

    res = system2("svn", c("export", "--force", paste("https://github.com/coseal/aslib_data/trunk/", name, sep = ""), path))
    if(res != 0) stop(paste("Scenario '", name, "' not found!", sep = ""))

    parseASScenario(path)
}
