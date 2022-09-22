#' Pack or unpack a SebasDB ID
#'
#' Pack or unpack an identifier for a file in SebasDB.
#'
#' @param id String containing an identifier for a file.
#' @param path String containing the relative rooted path to the result within the analysis project.
#' @param project String containing the project name. 
#' This should not contain \code{:}.
#' @param version String containing the project version.
#' This should not contain \code{@}.
#'
#' @return
#' \code{unpackID} will break down \code{id} and return a list containing character vectors of its components.
#' Note that the \code{"commit"} field is deprecated and is only returned for backwards compatibility.
#'
#' \code{packID} will pack the components into a single string.
#' If parallel components are supplied, a character vector is returned.
#'
#' @examples
#' unpackID("GPAX:asda/sds@12873128")
#' packID("GPAX", "asda/sds", "12873128")
#'
#' @author Aaron Lun
#' @export
unpackID <- function(id) {
    proj <- sub(":.*", "", id)
    commit <- sub(".*@", "", id)
    path <- sub("^[^:]+:(.+)@[^@]+$", "\\1", id)
    list(project=proj, path=path, version=commit, commit=commit)
}

#' @export
#' @rdname unpackID
packID <- function(project, path, version) {
    sprintf("%s:%s@%s", project, path, version)
}
