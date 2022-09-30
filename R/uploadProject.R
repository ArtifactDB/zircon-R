#' Upload a new version of a project
#'
#' Upload a new version of a project, given a staging directory containing files and their metadata.
#'
#' @inheritParams uploadFiles
#' @param ... Further arguments to pass to \code{\link{initializeUpload}}.
#' @param upload.args Further arguments to pass to \code{\link{uploadFiles}}.
#' @param complete.args Further arguments to pass to \code{\link{completeUpload}}.
#'
#' @return On success, \code{NULL} is returned invisibly.
#'
#' @author Aaron Lun
#' @seealso
#' \code{\link{uploadFiles}} and related functions, for the internal utilities.
#'
#' @export
uploadProject <- function(dir, url, project, version, files = list.files(dir, recursive=TRUE), ..., permissions=list(), upload.args=list(), complete.args=list(), user.agent=NULL) {
    start.url <- createUploadStartUrl(url, project, version)
    success <- FALSE
    info <- initializeUpload(dir, files, start.url, ..., user.agent=user.agent)

    parsed <- content(info, simplifyVector=TRUE, simplifyMatrix=TRUE, simplifyDataFrame=TRUE)
    on.exit(if (!success) abortUpload(url, parsed, user.agent=user.agent))

    upload.args <- c(list(dir=dir, url=url, initial=parsed, user.agent=user.agent), upload.args)
    do.call(uploadFiles, upload.args)

    complete.args <- c(list(url=url, initial=parsed, permissions=permissions, user.agent=user.agent), complete.args)
    do.call(completeUpload, complete.args)

    success <- TRUE
    invisible(NULL)
}
