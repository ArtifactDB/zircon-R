#' Retrieve metadata for a file and its children
#'
#' Retrieve metadata for a file and all its children from an ArtifactDB instance.
#' A file's set of children is defined as all other files that are referenced from the first file's metadata via the special \code{resource} property.
#'
#' @inheritParams getFileMetadata
#' @param ... Further arguments to pass to \code{\link{getFileMetadata}}.
#'
#' @return Named list where each entry corresponds to a file and contains the metadata for that file.
#' The first entry is always the metadata for \code{id}, and the remaining entries contain the metadata for children found in a depth-first search.
#'
#' @author Aaron Lun
#' @examples
#' # Not particularly interesting.
#' X <- getChildMetadata(example.id, url = example.url)
#' str(X)
#'
#' @export
getChildMetadata <- function(id, ...) {
    unpacked <- unpackID(id)
    parent <- getFileMetadata(id, ...)
    first <- list()
    first[[id]] <- parent
    children <- .inspect_metadata(unpacked[["project"]], unpacked[["version"]], parent, ...)
    c(first, children)
}

.inspect_metadata <- function(project, version, meta, ...) {
    output <- list()
    names(output) <- character(0)
    if (is.list(meta)) {
        return(output)
    }
    
    if (!is.null(names(meta))) {
        if ("resource" %in% names(meta)) {
            res <- meta$resource
            if ("type" %in% names(res) && "path" %in% names(res)) {
                output[[res$path]] <- getFileMetadata(packID(project, res$path, version), ...)
                return(output)
            }
        }
    }

    for (submeta in meta) {
        output <- c(output, .inspect_metadata(project, version, submeta, ...))
    }
    output
}
