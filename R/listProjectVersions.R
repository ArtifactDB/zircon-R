#' List project versions
#'
#' List the available versions of a project.
#'
#' @param project String containing the project name.
#' @param url String containing the URL to the ArtifactDB REST API.
#'
#' @return List containing \code{versions}, a character vector of available versions;
#' and \code{latest}, a string specifying the name of the latest version.
#'
#' @examples
#' listProjectVersions(example.project, example.url)
#' 
#' @seealso
#' \code{\link{listProjects}}, to obtain a listing of all projects.
#'
#' @author Aaron Lun
#' @export
#' @importFrom httr GET content
listProjectVersions <- function(project, url) {
    out <- authorizedVerb(GET, paste0(url, "/projects/", project, "/versions"))
    checkResponse(out)
    info <- content(out, simplifyVector=TRUE, simplifyMatrix=FALSE, simplify=FALSE)
    list(
        versions = vapply(info$agg, function(x) x[["_extra.version"]], ""),
        latest = info$latest[["_extra.version"]]
    )
}
