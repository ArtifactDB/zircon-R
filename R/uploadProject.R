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
#' @details
#' Use of this function will almost always require appropriate authentication/authorization with the target API.
#' Developers should ensure that \code{\link{identityHeaders}} and friends are set accordingly.
#'
#' Users should consider setting \code{expires} in \code{...} when testing uploads, to avoid creating a permanent copy of test files.
#' Check out \code{?\link{initializeUpload}} for more details.
#' 
#' Advanced users can reduce upload bandwidth and storage footprint by creating links to duplicate files between versions or projects.
#' Check out \code{?\link{initializeUpload}} for more details.
#'
#' @author Aaron Lun
#' @seealso
#' \code{\link{uploadFiles}} and related functions, for the internal utilities.
#'
#' @examples
#' # Creating a mock project.
#' src <- system.file("scripts", "mock.R", package="zircon")
#' source(src)
#' tmp <- tempfile()
#' createMockProject(tmp)
#' 
#' # Performing a basic upload:
#' \dontrun{
#' uploadProject(tmp, example.url, "test-zircon-upload", "foobar", expires=1)
#' }
#' @export
uploadProject <- function(dir, url, project, version, files = list.files(dir, recursive=TRUE), ..., permissions=list(), upload.args=list(), complete.args=list(), user.agent=NULL) {
    start.url <- createUploadStartURL(url, project, version)
    success <- FALSE
    info <- initializeUpload(dir, files, start.url, ..., user.agent=user.agent)

    parsed <- content(info, simplifyVector=TRUE, simplifyMatrix=FALSE, simplifyDataFrame=FALSE)
    on.exit(if (!success) abortUpload(url, parsed, user.agent=user.agent))

    upload.args <- c(list(dir=dir, url=url, initial=parsed, user.agent=user.agent), upload.args)
    do.call(uploadFiles, upload.args)

    complete.args <- c(list(url=url, initial=parsed, permissions=permissions, user.agent=user.agent), complete.args)
    do.call(completeUpload, complete.args)

    success <- TRUE
    invisible(NULL)
}
