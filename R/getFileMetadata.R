#' Retrieve a file or its metadata 
#'
#' Retrieve a file or its metadata from an ArtifactDB instance.
#'
#' @param id String containing the ArtifactDB identifier for a file.
#' This is a concatenated identifier involving the project name, file path and version,
#' i.e., \code{<project>:<path>@<version>} (see Examples).
#' @param url String containing the URL of the ArtifactDB REST endpoint.
#' @param raw Logical scalar indicating whether to obtain the raw metadata (in the format provided by the uploader, rather than indexed by ArtifactDB).
#' Setting this to \code{TRUE} avoids some loss of data from indexing at the cost of speed.
#' @param cache Function to perform caching, see Details.
#' If \code{NULL}, no caching is performed.
#' @param follow.link Logical scalar indicating whether to follow deduplicating links, if \code{id} is linked to another target resource. 
#' If \code{TRUE}, metadata for the target resource is returned.
#' @param follow.redirect Logical scalar indicating whether to follow uploader-provided redirections, if \code{id} is a placeholder that redirects to another target resource. 
#' If \code{TRUE}, metadata for the target resource is returned; otherwise, the metadata for the link itself is returned.
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
#' @importFrom httr content 
#' @importFrom jsonlite fromJSON
getFileMetadata <- function(id, url, cache=NULL, raw=FALSE, format=c("list", "text"), follow.link=FALSE, follow.redirect=TRUE, user.agent=NULL) {
    if (!is.null(cache)) {
        id <- resolveLatestID(id, url)
    }

    output <- .obtain_metadata(
        id, 
        url, 
        raw=raw, 
        follow.link=follow.link, 
        user.agent=user.agent,
        from.cache=function(path) fromJSON(path, simplifyVector=TRUE, simplifyDataFrame=FALSE, simplifyMatrix=FALSE),
        from.request=function(req) content(req, simplifyVector=TRUE, simplifyDataFrame=FALSE, simplifyMatrix=FALSE)
    )

    output <- .restore_schema(output)

    if (follow.redirect && startsWith(output[["$schema"]], "redirection/")) {
        for (found in output$redirection$targets) {
            if (found$type == "local") {
                unpacked <- unpackID(id)
                id2 <- packID(unpacked$project, found$location, unpacked$version)
                output <- getFileMetadata(id2, url, cache=cache, follow.link=follow.link, follow.redirect=follow.redirect, user.agent=user.agent)
                break
            }
        }
    }

    output
}

#' @importFrom httr content 
.obtain_metadata <- function(id, url, raw, follow.link, user.agent, from.cache, from.request) {
    endpoint <- paste(url, "files", URLencode(id, reserved=TRUE), "metadata", sep="/")
    options <- character(0)
    if (follow.link) {
        options <- c(options, "follow_link=true")
    }
    if (raw) {
        options <- c(options, "raw=true")
    }
    if (length(options)) {
        endpoint <- paste0(endpoint, "?", paste(options, collapse="&"))
    }

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
        output <- from.cache(path)
    } else {
        raw <- BASEFUN()
        output <- from.request(raw)
    }

    output
}

.restore_schema <- function(x) {
    if (!("$schema" %in% names(x)) && "_extra" %in% names(x)) {
        x[["$schema"]] <- x[["_extra"]][["$schema"]]
    }
    x
}
