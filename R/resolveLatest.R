#' Resolve the latest version
#'
#' Figure out the latest version of a project or file.
#' 
#' @param project String containing the project identifier.
#' @param id String containing a file identifier.
#' @param url String containing the URL of the ArtifactDB endpoint.
#'
#' @return 
#' For \code{resolveLatestVersion}, a string containing the latest version in its canonical form.
#'
#' For \code{resolveLatestID}, \code{id} is updated with any \code{"latest"} version alias replaced by its canonical form.
#'
#' @author Aaron Lun
#' @examples
#' resolveLatestVersion(example.project, example.url)
#'
#' unpacked <- unpackID(example.id)
#' unpacked$version <- "latest"
#' latest.id <- do.call(packID, unpacked)
#' resolveLatestID(latest.id, example.url)
#' 
#' @export
#' @importFrom httr content
resolveLatestVersion <- function(project, url) {
    endpoint <- paste(url, "projects", project, "versions", sep="/")
    output <- authorizedVerb(GET, url=endpoint)
    checkResponse(output)
    content(output)[["latest"]][["_extra.version"]]
}

#' @export
resolveLatestID <- function(id, url) {
    if (endsWith(id, "@latest")) {
        unpacked <- unpackID(id)
        if (unpacked$version == "latest") {
            unpacked$version <- resolveLatestVersion(unpacked$project, url)
        }
        do.call(packID, unpacked)
    } else {
        id
    }
}
