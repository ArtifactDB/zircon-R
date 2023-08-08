#' Caching functions 
#'
#' This implements a few possible \code{cache} functions for use in \code{\link{getFileMetadata}} and friends.
#'
#' @param cache For \code{biocCache}, a BiocFileCache object.
#' 
#' For \code{simpleCache}, a string containing a path to a cache directory.
#' @param key,save See \code{\link{getFileMetadata}} for details on the caching function.
#' @param update Logical scalar indicating whether assets in the cache should be forcibly updated.
#' Useful for resolving corrupted caches or incomplete downloads.
#'
#' @return String containing a path to the cached resource inside \code{cache}.
#'
#' @details
#' For \code{simpleCache}, caching is done by downloading directly to the file system with no further tracking.
#'
#' For \code{biocCache}, caching is based on \pkg{BiocFileCache}, which tracks all calls via a central SQLite database.
#'
#' In all cases, downloads and access are protected by \pkg{filelock}, so multiple processes can query/update the cache safely.
#'
#' @author Aaron Lun
#'
#' @examples
#' library(BiocFileCache)
#' tmp <- tempfile()
#' bfc <- BiocFileCache(tmp, ask=FALSE)
#' cache.fun <- function(key, save) biocCache(bfc, key, save)
#' 
#' # First query hits the cache:
#' getFile(example.id, example.url, cache = cache.fun)
#'
#' # Second query re-uses the cache:
#' getFile(example.id, example.url, cache = cache.fun)
#'
#' @seealso
#' \code{\link{getFileMetadata}} for more details on the caching function.
#' 
#' @name caching
NULL

#' @export
#' @rdname caching
#' @importFrom utils URLencode
simpleCache <- function(cache, key, save, update=FALSE) {
    path <- file.path(cache, URLencode(key, reserved=TRUE, repeated=TRUE))

    # Acquiring a path lock so that processes don't use the downloaded file
    # after it started but before it's finished.
    path.lock <- paste0(path, "_zircon-LOCK")
    plock <- filelock::lock(path.lock)
    on.exit(filelock::unlock(plock), add=TRUE)

    if (!file.exists(path) || update) {
        success <- FALSE
        on.exit({
            if (!success) {
                unlink(path, force=TRUE)
            }
        }, add=TRUE)
        save(path)
        success <- TRUE
    }

    path
}

#' @export
#' @rdname caching
biocCache <- function(cache, key, save, update=FALSE) {
    lockfile <- file.path(BiocFileCache::bfccache(cache), "zircon-LOCK")
    lck <- filelock::lock(lockfile)
    on.exit({
        if (!is.null(lck)) {
            filelock::unlock(lck)
        }
    }, add=TRUE)
    hit <- BiocFileCache::bfcquery(cache, key, field="rname", exact=TRUE)

    must.fire <- FALSE
    if (nrow(hit) >= 1L) {
        if (nrow(hit) > 1L) {
            warning("detected and removed duplicate copies of '", key, "'")
            BiocFileCache::bfcremove(cache, hit$rid[-1])
            hit <- hit[1,,drop=FALSE]
        }
        path <- file.path(BiocFileCache::bfccache(cache), hit$rpath)
    } else {
        path <- unname(BiocFileCache::bfcnew(cache, key))
        must.fire <- TRUE
    }

    filelock::unlock(lck)
    lck <- NULL

    # Acquiring a path lock so that processes don't use the downloaded file
    # after it started but before it's finished.
    path.lock <- paste0(path, "_zircon-LOCK")
    plock <- filelock::lock(path.lock)
    on.exit(filelock::unlock(plock), add=TRUE)

    if (must.fire || !file.exists(path) || update) {
        success <- FALSE
        on.exit({
            if (!success) {
                unlink(path, force=TRUE)
            }
        }, add=TRUE)
        save(path)
        success <- TRUE
    }

    path
}
