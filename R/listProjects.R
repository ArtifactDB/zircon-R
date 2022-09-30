#' List projects 
#'
#' List the available projects that are visible to the current user.
#'
#' @param url String containing the URL to the ArtifactDB REST API.
#' @param number Number of projects to report.
#'
#' @return Named list of character vectors.
#' Each character vector corresponds to a project and is named after that project;
#' the vector itself contains the names of all versions of that project.
#' The list is of length no greater than \code{number}.
#'
#' @examples
#' listProjects(example.url, number = 10)
#' 
#' @seealso
#' \code{\link{listProjectVersions}}, to obtain a listing of one project's versions.
#'
#' @author Aaron Lun
#' @export
#' @importFrom utils head
#' @importFrom httr GET content headers
listProjects <- function(url, number = 50) {
    collected <- list()
    URL <- paste0(url, "/projects")
    explicit.number <- !missing(number)

    repeat {
        output <- .follow_redirects_faithfully(GET, url=URL)
        checkResponse(output)

        info <- content(output, simplifyVector=TRUE, simplifyMatrix=FALSE, simplify=FALSE)
        for (x in info$results) {
            collected[[x$project_id]] <- vapply(x$aggs, function(y) y[["_extra.version"]], "")
        }

        if (length(collected) > number) {
            break
        }

        link.info <- headers(output)$link
        URL <- .parse_link_url(url, link.info)
        if (is.null(URL)) {
            break
        }
    }

    if (length(collected) > number && !explicit.number) {
        warning("truncated project listing to the first ", number, " results")
        collected <- head(collected, number)
    }

    collected
}
