#' Retrieve a file or its metadata 
#'
#' Retrieve metadata for a file or the file itself from an ArtifactDB using its REST endpoints.
#'
#' @param id String containing the ArtifactDB identifier for a file.
#' This is a concatenated identifier involving the project name, file path and version,
#' i.e., \code{<project>:<path>@<version>} (see Examples).
#' @param url String containing the URL of the ArtifactDB REST endpoint.
#' @param cache Function to perform caching, see Details.
#' If \code{NULL}, no caching is performed.
#' @param follow.links Logical scalar indicating whether to follow links, if \code{id} is a link to another target resource. 
#' If \code{TRUE}, metadata for the target resource is returned; otherwise, metadata for the link itself is returned.
#' @param user.agent String containing the user agent, see \code{\link{authorizedVerb}}.
#'
#' @return
#' A list containing metadata for the specified file.
#' The contents will depend on the schema used by the ArtifactDB at \code{url}.
#'
#' @author Aaron Lun
#'
#' @details
#' The caching function should accept:
#' \itemize{
#' \item \code{key}, the endpoint URL used to acquire the requested resource.
#' This is used as a unique key for the requested metadata.
#' The caching mechanism should convert this into a suitable file path, e.g., via \code{\link{URLencode}}.
#' It can be assumed that any \code{latest} version aliases in the input \code{id} have already been resolved.
#' \item \code{save}, a function that accepts a single string containing a path to a local file system generated from \code{key}.
#' It will perform the request to the AritfactDB REST API, save the response to the specified path and return nothing.
#' The caching function should call \code{save} with a suitable key-derived path if \code{key} does not already exist in the cache.
#' }
#' The caching function itself should return the path used in \code{save}.
#' See \code{\link{biocCache}} for one possible implementation based on Bioconductor's \pkg{BiocFileCache} package.
#'
#' @examples
#' # No caching:
#' X <- getFileMetadata(example.id, url = example.url)
#' str(X)
#'
#' # Simple caching in the temporary directory:
#' tmp.cache <- file.path(tempdir(), "zircon-cache")
#' dir.create(tmp.cache)
#' cache.fun <- function(key, save) {
#'     path <- file.path(tmp.cache, URLencode(key, reserved=TRUE, repeated=TRUE))
#'     if (!file.exists(path)) {
#'         save(path)
#'     } else {
#'         cat("cache hit!\n")
#'     }
#'     path
#' }
#' X1 <- getFileMetadata(example.id, example.url, cache = cache.fun)
#' X2 <- getFileMetadata(example.id, example.url, cache = cache.fun) # just re-uses the cache
#'
#' @seealso
#' \code{\link{packID}}, to create \code{id} from various pieces of information.
#'
#' \code{\link{identityAvailable}}, to inject authorization information into the API request.
#'
#' @export
#' @rdname getFileMetadata
#' @importFrom httr GET content write_disk
#' @importFrom jsonlite fromJSON
getFileMetadata <- function(id, url, cache=NULL, follow.links=TRUE, user.agent=NULL) {
    if (!is.null(cache)) {
        id <- resolveLatestID(id, url)
    }

    endpoint <- .get_file_metadata_url(id, url, follow.links=follow.links)
    BASEFUN <- function(...) {
        output <- authorizedVerb(GET, url=endpoint, ..., user.agent=user.agent)
        checkResponse(output)
        output
    }

    if (!is.null(cache)) {
        path <- cache(
            key = endpoint,
            save = function(path) BASEFUN(write_disk(path, overwrite=TRUE))
        )
        output <- fromJSON(path, simplifyVector=TRUE, simplifyDataFrame=FALSE, simplifyMatrix=FALSE)
        return(output)
    }

    raw <- BASEFUN()
    content(raw, simplifyVector=TRUE, simplifyDataFrame=FALSE, simplifyMatrix=FALSE)
}

.get_file_metadata_url <- function(x, u, follow.links) {
    endpoint <- paste(u, "files", URLencode(x, reserved=TRUE), "metadata", sep="/")
    if (follow.links) {
        endpoint <- paste0(endpoint, "?follow_link=true")
    }
    endpoint
}
