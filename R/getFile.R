#' Retrieve a file 
#'
#' Retrieve a file from an ArtifactDB using its REST endpoints.
#'
#' @param id String containing the ArtifactDB identifier for a file.
#' This is a concatenated identifier involving the project name, file path and version,
#' i.e., \code{<project>:<path>@<version>} (see Examples).
#' @param expiry Integer scalar specifying the time until expiry of the link, in seconds.
#' Defaults to 120 if \code{NULL}; maximum value is 86400 (24 hours).
#' @param path String containing the path to save the file.
#' This is only used if \code{cache} is not supplied.
#' Defaults to a temporary path if \code{NULL}.
#' @param url String containing the URL of the ArtifactDB REST endpoint.
#' @param cache Function to use for caching the result, see \code{\link{getFileMetadata}} for the requirements.
#' If \code{NULL}, no caching is performed.
#' @param follow.links Logical scalar indicating whether to search for links to duplicate files, see Details.
#' @param user.agent String containing the user agent, see \code{\link{authorizedVerb}}.
#'
#' @return
#' For \code{getFileURL}, a string containing a URL that can be used to download the specified file.
#'
#' For \code{getFile}, a string containing a path to a cached file downloaded from the ArtifactDB instance.
#'
#' @author Aaron Lun
#'
#' @details
#' ArtifactDB instances support linking between identical files to avoid storing unnecessary duplicates.
#' If \code{cache} is provided and \code{follow.links=TRUE}, \code{getFile} will follow the links to determine whether the requested file is the same as any previously cached files.
#' This avoids downloading another copy of the file if one is already available under a linked identifier.
#' If no local duplicate can be found, \code{getFile} will download the earliest version of the file among the set of duplicates, 
#' and then populate all other duplicates in the cache directory with hard links or copies.
#' If this fails or if \code{cache=NULL}, it will just download the requested file directly.
#' 
#' @examples
#' getFileURL(example.id, url = example.url)
#'
#' X <- getFile(example.id, url = example.url)
#' readLines(X) # as we know it's a text file.
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
#' getFile(example.id, example.url, cache = cache.fun)
#' getFile(example.id, example.url, cache = cache.fun) 
#'
#' @seealso
#' \code{\link{packID}}, to create \code{id} from various pieces of information.
#'
#' @export
#' @importFrom methods is
#' @importFrom utils download.file
getFile <- function(id, url, path=NULL, cache=NULL, follow.links=TRUE, user.agent=NULL) {
    if (!is.null(cache)) {
        id <- resolveLatestID(id, url)
        if (follow.links) {
            return(.get_original_linked_file(id, url, cache, user.agent))
        }
    }

    endpoint <- .get_raw_file_url(id, url)
    SAVEFUN <- .generate_saver(endpoint, user.agent=user.agent)

    if (!is.null(cache)) {
        return(cache(endpoint, SAVEFUN)) 
    }

    if (is.null(path)) {
        path <- tempfile()
    }
    SAVEFUN(path)
    return(path)
}

#' @importFrom utils URLencode
.get_raw_file_url <- function(id, url) {
    file.path(url, "files", URLencode(id, reserved=TRUE))
}

#' @importFrom httr GET config headers
.get_presigned_url <- function(target) {
    link.data <- authorizedVerb(GET, url=target, config(followlocation = 0L), allow.redirect=TRUE)
    checkResponse(link.data, allow.redirect=TRUE)
    headers(link.data)$location
}

#' @importFrom httr write_disk
.generate_saver <- function(endpoint, user.agent) {
    if (is.null(user.agent)) {
        user.agent <- .raw_user_agent()
    }
    function(path) {
        old <- options(HTTPUserAgent=user.agent)
        on.exit(options(old))
        final <- .get_presigned_url(endpoint)
        download.file(final, path, mode="wb")
    }
}

.get_original_linked_file <- function(id, url, cache, user.agent) {
    output <- try(getFileMetadata(id, url=url, cache=cache, follow.links=TRUE, user.agent=user.agent), silent=TRUE)

    endpoint <- .get_raw_file_url(id, url)

    # Trying to fetch the linked file, if it's available. We recurse through
    # the chain of links until we get to the earliest file (or the chain 
    # was broken somewhere), at which point we just download the file. 
    if (!is(output, "try-error")) {
        link.id <- output[["_extra"]][["link"]][["artifactdb"]]

        if (!is.null(link.id)) {
            linked <- try(.get_original_linked_file(link.id, url=url, cache=cache, user.agent=user.agent), silent=TRUE)

            if (!is(linked, "try-error")) {
                # This has the effect of populating all intermediate linking entries.
                return(cache(endpoint, function(path) file.link(linked, path) || file.copy(linked, path)))
            }
        }
    }

    SAVEFUN <- .generate_saver(endpoint, user.agent=user.agent)
    cache(endpoint, SAVEFUN)
}

#' @export
#' @rdname getFile
#' @importFrom httr GET config headers
getFileURL <- function(id, url, expiry=NULL) {
    URL <- .get_raw_file_url(id, url)

    if (!is.null(expiry)) {
        expiry <- as.integer(expiry)
        URL <- paste0(URL, "?expires_in=", expiry)
    }

    .get_presigned_url(URL)
}
