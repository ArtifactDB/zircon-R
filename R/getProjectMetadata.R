#' Retrieve metadata for a project 
#'
#' Retrieve metadata for a project from an ArtifactDB using its REST endpoints.
#'
#' @param project String containing the name of a project.
#' @param url String containing the URL of the ArtifactDB REST endpoint.
#' @param version String specifying the version or revision of the project for which to retrieve metadata.
#'
#' @return
#' A list is returned containing metadata for all files from a project.
#' If \code{version} is specified, files are only returned for that version of the project.
#'
#' @author Aaron Lun
#'
#' @examples
#' # Project-wide metadata for a single version:
#' out <- getProjectMetadata(example.project, version=example.version, url=example.url)
#' length(out)
#'
#' # Project-wide metadata, all versions:
#' out <- getProjectMetadata(example.project, url = example.url)
#' length(out)
#'
#' @seealso
#' \code{\link{getFileMetadata}}, to get metadata for a specific file.
#'
#' @export
#' @importFrom httr GET content headers
getProjectMetadata <- function(project, url, version=NULL) {
    URL <- file.path(url, "projects", project)
    if (!is.null(version)) {
        URL <- file.path(URL, "version", version)
    }
    URL <- file.path(URL, "metadata")

    collected <- list()
    repeat {
        output <- .follow_redirects_faithfully(GET, url=URL)
        checkResponse(output)

        harvest <- content(output, simplifyVector=TRUE, simplifyDataFrame=FALSE, simplifyMatrix=FALSE)
        harvest <- harvest$results
        if (length(harvest)==0) {
            break
        }

        collected <- c(collected, harvest)

        link.info <- headers(output)$link
        URL <- .parse_link_url(url, link.info)
        if (is.null(URL)) {
            break
        }
    }

    collected
}
