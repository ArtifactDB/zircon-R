#' Get/set permissions for a project 
#'
#' Get or set permissions for a project from an ArtifactDB using its REST endpoints.
#'
#' @param project String containing the name of a project.
#' @param url String containing the URL of the ArtifactDB REST endpoint.
#' @param version String specifying the version or revision of the project for which to set/get permissions.
#' @param owners Character vector of unix IDs or d-lists to set as/append to/remove from the list of owners for \code{project}.
#' @param viewers Character vector of unix IDs or d-lists to set as/append to/remove from the list of viewers for \code{project}.
#' @param action String indicating the action to use to modify the permissions.
#' @param public Logical scalar indicating whether the results should be made public.
#'
#' @return
#' For \code{getPermissions}, a list is returned containing permission information for a project (or one of its versions).
#' This contains the fields:
#' \itemize{
#' \item \code{scope}, a string specifying the scope to which the permissions apply. 
#' Usually \code{"project"}.
#' \item \code{owners}, a character vector identifying the project owners in the form of unix IDs or d-lists.
#' Owners can read all files and edit the permissions.
#' \item \code{viewers}, a character vector identifying all users with read access to the project files.
#' \item \code{read_access}, a string specifying who is allowed to view the results.
#' This can be everyone (\code{"public"}), all \code{"authenticated"} users, \code{"viewers"} and owners, \code{"owners"} only, or no one at all (\code{"none"}).
#' \item \code{write_access}, a string specifying who is allowed to edit the permissions.
#' This may be any of values described for \code{read_access}, though it is rarely anything other than \code{"owners"}.
#' }
#'
#' For \code{.setPermissions}, the permissions are modified for the project and \code{NULL} is invisibly returned.
#' Note that there may be some delay in processing the request; the project itself will be locked until the processing is complete.
#'
#' @details
#' Only users listed in the project's \code{owners} are allowed to modify the permissions for a project.
#' Users in \code{viewers} (or all users, for public projects) are allowed to view but not modify a project's permissions.
#'
#' If \code{action="append"}, any specified \code{owners} are appended to the existing list of owners in the permissions for \code{project}.
#' If \code{action="remove"}, any specified \code{owners} are removed from the existing list. 
#' If \code{action="set"}, any specified \code{owners} replace the existing list (unless \code{owners=NULL}, in which case no action is taken).
#' The same logic applies to \code{viewers}.
#'
#' If \code{public=TRUE}, the project is made public, i.e., \code{read_access} is set to \code{"public"}.
#' If \code{public=FALSE}, the project is viewable only by the users specified in the \code{"viewers"} field.
#' The default of \code{NULL} will preserve any existing value for \code{read_access}.
#' 
#' If \code{version} is specified, these functions will get/set permissions for a specific version of a project.
#' By default, if no permissions are placed on individual versions, they inherit their permissions from the project-level settings.
#' This option is only applicable if version-level permissions are supported by the API at \code{url}.
#'
#' @author Aaron Lun
#'
#' @examples
#' getPermissions(example.project, url = example.url)
#'
#' \dontrun{
#' # Add Michael Lawrence to the viewers.
#' setPermissions(example.project, url = example.url,
#'     viewers="lawremi", action="append")
#'
#' # Remove Michael Lawrence from the viewers.
#' setPermissions(example.project, url = example.url,
#'     viewers="lawremi", action="remove")
#' }
#'
#' @export
#' @importFrom httr GET content
getPermissions <- function(project, url, version=NULL) {
    url <- file.path(url, "projects", project)
    if (!is.null(version)) {
        url <- file.path(url, "version", version)
    }
    url <- file.path(url, "permissions")

    out <- authorizedVerb(GET, url=url)
    checkResponse(out)

    output <- content(out, simplifyVector=TRUE, simplifyMatrix=FALSE, simplifyDataFrame=FALSE)
    output$viewers <- as.character(output$viewers)
    output$owners <- as.character(output$owners)

    output
}

#' @export
#' @rdname getPermissions
#' @importFrom httr add_headers PUT content_type_json
#' @importFrom jsonlite toJSON
setPermissions <- function(project, url, owners=NULL, viewers=NULL, action=c("append", "set", "remove"), public=NULL, version=NULL) { 
    previous <- getPermissions(project, url, version=version)
    action <- match.arg(action)

    if (is.null(owners)) {
        owners <- previous$owners
    } else if (action=="append") {
        owners <- union(previous$owners, owners)
    } else if (action=="remove") {
        owners <- setdiff(previous$owners, owners)
    } 

    if (is.null(viewers)) {
        viewers <- previous$viewers
    } else if (action=="append") {
        viewers <- union(previous$viewers, viewers)
    } else if (action=="remove") {
        viewers <- setdiff(previous$viewers, viewers)
    } 

    if (is.null(public)) {
        public <- previous$read_access == "public"
    }

    url <- file.path(url, "projects", project)
    if (!is.null(version)) {
        url <- file.path(url, "version", version)
    }
    url <- file.path(url, "permissions")

    # Defaults to NULL in the backend if not specified.
    permissions <- list()
    if (!is.null(owners)) {
        permissions$owners <- I(owners)
    }
    if (!is.null(viewers)) {
        permissions$viewers <- I(viewers)
    }
    if (!is.null(public)) {
        permissions$read_access <- if (public) "public" else "viewers"
    }

    # httr removes empty fields, so we need to manually serialize this.
    out <- .follow_redirects_faithfully(PUT, 
        url=url, 
        body=toJSON(permissions, auto_unbox=TRUE),
        encode="raw",
        content_type_json(),
        .user_agent()
    )
    checkResponse(out)
    
    invisible(NULL)
}
