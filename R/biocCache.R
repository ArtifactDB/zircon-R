#' Use the Bioconductor caching function
#'
#' This implements one possible \code{cache} function for use in \code{\link{getFileMetadata}} and friends.
#' Caching is based on \pkg{BiocFileCache} with thread-safety enforced by \pkg{filelock}.
#'
#' @param cache A BiocFileCache object.
#' @param key,save See \code{\link{getFileMetadata}} for details on the caching function.
#' @param update Logical scalar indicating whether assets in the cache should be forcibly updated.
#' Useful for resolving corrupted caches or incomplete downloads.
#'
#' @return String containing a path to the cached resource inside \code{cache}.
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
#' @export
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


