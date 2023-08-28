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
#' @param user.agent String containing the user agent, see \code{\link{authorizedVerb}}.
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
cloneProjectVersion <- function(dir, url, project, version, link.only=FALSE, cache=NULL, user.agent=NULL) {
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

        dest <- file.path(dir, metapath)
        fid <- packID(project, m$path, version)
        output <- .obtain_metadata(
            id=fid,
            url=url, 
            cache=cache,
            raw=TRUE,
            follow.link=FALSE,
            user.agent=user.agent,
            from.cache=function(path) file.copy(path, dest),
            from.request=function(req) writeBin(content(req, as="raw"), dest)
        )

        if (!startsWith(m[["$schema"]], "redirection/") && !pure.meta) {
            if (link.only) {
                createPlaceholderLink(dir, m$path, fid)
            } else {
                final <- file.path(dir, m$path)
                if (is.null(cache)) {
                    getFile(fid, url=url, path=final)
                } else {
                    # Making an explicit copy to avoid mutation of the cache
                    # when someone modifies the clone.
                    cached <- getFile(fid, url=url, cache=cache, user.agent=user.agent)
                    if (!file.copy(cached, final)) {
                        stop("failed to copy cached file during cloning")
                    }
                }
            }
        }
    }
}
