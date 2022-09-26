#' Retrieve a file 
#'
#' Retrieve a file from an ArtifactDB using its REST endpoints.
#'
#' @param id String containing the ArtifactDB identifier for a file.
#' This is a concatenated identifier involving the project name, file path and version,
#' i.e., \code{<project>:<path>@<version>} (see Examples).
#' @param expiry Integer scalar specifying the time until expiry of the link, in seconds.
#' Defaults to 120 if \code{NULL}; maximum value is 86400 (24 hours).
#' @param url String containing the URL of the ArtifactDB REST endpoint.
#' @param cache Function to use for caching the result, see \code{\link{getFileMetadata}} for the requirements.
#' If \code{NULL}, no caching is performed.
#' @param follow.links Logical scalar indicating whether to search for links to duplicate files, see Details.
#' @param notify.redirect See \code{\link{.getFileMetadata}}.
#'
#' @return
#' For \code{getFileURL}, a string containing a URL that can be used to download the specified file.
#'
#' For \code{getFile}, a string containing a path to a cached file downloaded from the ArtifactDB instance.
#'
#' @author Aaron Lun
#'
#' @details
#' ArtifactDB instances support deduplication via links between identical files.
#' If \code{follow.links=TRUE}, \code{getFile} will follow the links to determine whether the requested file is the same as any previously cached files, so as to avoid redownloading a duplciate file.
#' If no local duplicate can be found, it will attempt to download the earliest version of the file among the set of duplicates, and then create hard links to the remaining versions in the cache directory.
#' If this fails or if \code{cache=FALSE}, it will just download the requested file directly.
#' 
#' @examples
#' getFileURL(example.file.id, url = example.url)
#'
#' getFile(example.file.id, url = example.url)
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
#' getFile(example.file.id, example.url, cache = cache.fun)
#' getFile(example.file.id, example.url, cache = cache.fun) 
#'
#' @seealso
#' \code{\link{packID}}, to create \code{id} from various pieces of information.
#'
#' @export
#' @importFrom methods is
#' @importFrom utils download.file
getFile <- function(id, url, cache=NULL, follow.links=TRUE, user.agent=NULL) {
    if (!is.null(cache) && follow.links) {
        return(.get_original_linked_file(id, url, cache, user.agent))
    }

    helpers <- .generate_cacheable(id, url, user.agent=user.agent)
    SAVEFUN <- helpers$save
    if (is.null(cache)) {
        tmp <- tempfile()
        SAVEFUN(tmp)
        return(tmp)
    }

    URL <- helpers$key
    return(cache(URL, SAVEFUN)) 
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
.generate_cacheable <- function(id, url, user.agent) {
    URL <- .get_raw_file_url(id, url)
    list(key = URL, save = function(path) {
        final <- .get_presigned_url(URL)
        download.file(final, path, mode="wb")
    })
}

.get_original_linked_file <- function(id, url, cache, user.agent) {
    # Don't try to set the cache.id to something constant, as the
    # contents will not be the same for different 'id', and it will
    # also be different between files and metadata. Just let the
    # default happen with the url-based ID.
    #
    # We also set follow.links=FALSE to force it to take one link at a
    # time, to ensure that we check for the presence of a cached file 
    # for all IDs in the link chain. Otherwise we might skip one and
    # be forced to unnecessarily download the file.
    output <- try(getFileMetadata(id, url=url, cache=cache, follow.links=FALSE, user.agent=user.agent), silent=TRUE)

    # Trying to fetch the linked file, if it's available. We recurse through
    # the chain of links until we get to the earliest file (or the chain 
    # was broken somewhere), at which point we just download the file. 
    if (!is(output, "try-error")) {
        link.id <- output[["_extra"]][["link"]][["artifactdb"]]

        if (!is.null(link.id)) {
            linked <- try(.get_original_linked_file(link.id, url=url, cache=cache, user.agent=user.agent), silent=TRUE)

            if (!is(linked, "try-error")) {
                # This has the effect of populating all intermediate linking entries.
                if (file.link(linked, target) || file.copy(linked, target)) {
                    return(target)
                }
            }
        }
    }

    helpers <- .generate_cacheable(id, url, user.agent=user.agent)
    cache(helpers$key, helpers$save)
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
