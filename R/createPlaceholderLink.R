#' Create a placeholder link
#'
#' Create a placeholder file that links to an ArtifactDB ID.
#' This is by \code{\link{initializeUpload}} to automatically create deduplicating links. 
#'
#' @param dir String containing the path to the project directory.
#' @param path String containing the relative path of the file in \code{dir} to be linked from.
#' @param id String containing the ArtifactDB identifier of the file to be linked to.
#' 
#' @return For \code{createLink}, a symbolic link is created at \code{path} inside \code{dir}, with the \code{id} string embedded in the name of the destination file.
#' \code{NULL} is invisibly returned.
#'
#' For \code{extractLinkID}, a string is returned containing the ArtifactDB identifier used to create the link.
#' 
#' @details
#' The placeholder is implemented as a dangling symlink where the \code{id} is embedded into the destination path.
#' This distinguishes the placeholders from regular files and non-broken symlinks to existing targets,
#' allowing \code{\link{initializeUpload}} to safely assume that the placeholder represents a link to an ArtifactDB resource.
#' 
#' @author Aaron Lun
#'
#' @examples
#' tmp <- tempfile()
#' dir.create(tmp)
#'  
#' createPlaceholderLink(tmp, "boo.txt", example.id)
#' list.files(tmp)
#'
#' extractLinkedID(tmp, "boo.txt")
#'
#' @seealso
#' \code{\link{initializeUpload}}, where the placeholders are automatically detected for linking.
#'
#' @export
#' @importFrom utils URLencode
createPlaceholderLink <- function(dir, path, id) {
    tmp <- tempfile(pattern=paste0(URLencode(id, reserved=TRUE), ":"))
    write(file=tmp, character(0))
    if (!file.symlink(tmp, file.path(dir, path))) {
        stop("failed to create a symlink to '", tmp, "'")
    }
    unlink(tmp) # force it to be dangling.
}

#' @export
#' @rdname createPlaceholderLink
extractLinkedID <- function(dir, path) {
    link.target <- Sys.readlink(file.path(dir, path))
    if (link.target == "") {
        return(NULL)
    } else {
        .extract_link_id(link.target)
    }
}

#' @importFrom utils URLdecode
.extract_link_id <- function(link.target) {
    if (file.exists(link.target)) {
        stop("placeholder should be a dangling symlink")
    }
    URLdecode(sub(":.*", "", basename(link.target)))
}
