#' Clone a version of project
#'
#' Clone the contents of a project version onto the local filesystem.
#'
#' @param dir String containing a path to the destination directory.
#' @param url String containing the URL to the REST API.
#' @param project String containing the project name.
#' @param version String containing the project version.
#' @param link.only Logical scalar indicating whether to create placeholder links instead of copies of the non-metadata files.
#' @param cache Caching function to use in \code{\link{getFile}} or \code{\link{getFileMetadata}}.
#' 
#' @return A directory is created at \code{dir} with the contents of the specified project version.
#' \code{NULL} is invisibly returned.
#'
#' @details
#' If \code{link.only=TRUE} is specified, all non-metadata files are replaced by placeholder links using \code{\link{createPlaceholderLink}}.
#' This avoids the download while retaining the project structure for further modifications, e.g., using \code{\link{initializeUpload}}.
#'
#' If \code{cache} is provided, metadata documents are first cached and then copied to \code{dir}.
#' This avoids re-downloads of the same documents in subsequent \code{\link{cloneProjectVersion}} calls.
#' If \code{link.only=FALSE}, non-metadata files will also be cached.
#' 
#' @examples
#' tmp <- tempfile()
#' cloneProjectVersion(tmp, example.url, example.project, example.version)
#' list.files(tmp, recursive=TRUE)
#'
#' @author Aaron Lun
#' @seealso
#' \code{\link{createPlaceholderLink}}, for the link creation procedure.
#' 
#' @export
cloneProjectVersion <- function(dir, url, project, version, link.only=FALSE, cache=NULL) {
    dir.create(dir, showWarnings=FALSE)
    meta <- getProjectMetadata(project, version=version, url=url)

    for (m in meta) {
        mdir <- file.path(dir, dirname(m$path))
        if (!file.exists(mdir)) {
            dir.create(mdir, showWarnings=FALSE, recursive=TRUE)
        }

        metapath <- m$path
        pure.meta <- endsWith(metapath, ".json")
        if (!pure.meta) {
            metapath <- paste0(metapath, ".json")
        }

        if (startsWith(m[["$schema"]], "redirection/")) {
            m <- m[setdiff(names(m), "_extra")]

            # Dealing with these guys separately, because /files will just
            # redirect to the destination file, but we want to actually save
            # the redirection metadata. Round-trip shouldn't be a problem here.
            write(jsonlite::toJSON(m, auto_unbox=TRUE), file.path(dir, metapath))

        } else {
            # Using the /files endpoint to download the metadata documents,
            # which is technically allowed. This avoids the _extra stuff and
            # potential problems from round-tripping JSON through R (namely,
            # handling of length-1 vectors and numerical precision).
            .cache_copy(
                packID(project, metapath, version), 
                url=url, 
                cache=cache, 
                final=file.path(dir, metapath)
            )

            if (!pure.meta) {
                fid <- packID(project, m$path, version)
                if (link.only) {
                    createPlaceholderLink(dir, m$path, fid)
                } else {
                    .cache_copy(fid, url=url, cache=cache, final=file.path(dir, m$path))
                }
            }
        }
    }
}

.cache_copy <- function(id, url, cache, final) {
    if (is.null(cache)) {
        getFile(id, url=url, path=final)
    } else {
        # Making an explicit copy to avoid mutation of the cache
        # when someone modifies the clone.
        cached <- getFile(id, url=url, cache=cache)
        if (!file.copy(cached, final)) {
            stop("failed to copy cached file during cloning")
        }
    }
}
