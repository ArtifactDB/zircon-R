#' Perform authorized HTTP requests
#'
#' Perform authorized HTTP requests to an ArtifactDB REST endpoint.
#'
#' @param FUN A \pkg{httr} function defining a verb, e.g., \code{\link{GET}}.
#' @param ... Further arguments to pass to \code{FUN}.
#' @param allow.redirect Logical scalar indicating whether a redirect should not be considered a failure.
#' @param user.agent String specifying the user agent, defaults to the current R installation and \pkg{zircon} version.
#' @param x Response list from \pkg{httr} verb functions.
#' @param fun Functions that check the identity and create HTTP request headers, see Details.
#'
#' @return 
#' For \code{authorizedVerb}, the output of \code{FUN} without checking for errors.
#'
#' For \code{checkResponse}, an error is raised with an appropriate message if the request failed.
#' Otherwise, an invisible \code{NULL}.
#'
#' For \code{identityAvailable} and \code{identityHeaders}, if \code{fun} is missing, the current function is returned.
#' If \code{fun} is supplied, it is used as the checker/creator function inside \code{authorizedVerb}, and the \emph{previous} value of the function is invisibly returned.
#'
#' @details
#' The \code{fun} in \code{identityAvailable} should accept no arguments and should return a logical scalar specifying whether a user identity is available for the current R session.
#'
#' The \code{fun} in \code{identityHeaders} should accept no arguments and should return a named list of HTTP request headers that authenticates the user to the ArtifactDB backend. 
#' The exact nature of these headers depends on the authentication system used by the backend, e.g., \code{Authorization} headers with JSON web tokens, SSH keys, or others.
#' If the corresponding credentials are not available, \code{identityHeaders} should prompt the user to create them (or fail outright in non-interactive sessions).
#'
#' If \code{identityAvailable} is not available or returns \code{FALSE}, \code{authorizedVerb} will attempt to perform an unauthenticated request.
#' If this fails and \code{identityHeader} is available, it will repeat the request after calling \code{identityHeader} to create authentication headers.
#' This process avoids burdening the user with authentication issues when requesting publicly available resources, falling back to some authentication process as required.
#'
#' @author Aaron Lun
#' @examples
#' library(httr)
#' URL <- file.path(example.url, "files", URLencode(example.id, reserved=TRUE))
#' authorizedVerb(GET, url=URL)
#'
#' # Triggering failures:
#' URL2 <- file.path(example.url, "files", "something_is_absent")
#' out <- authorizedVerb(GET, url=URL2)
#' try(.checkResponse(out))
#' 
#' @export
#' @rdname authorizedVerb
#' @importFrom httr add_headers
authorizedVerb <- function(FUN, ..., user.agent=NULL, allow.redirect=FALSE) {
    id.fun <- identityAvailable()
    head.fun <- identityHeaders()

    args <- list(..., .user_agent(user.agent))
    identified <- if (is.null(id.fun)) FALSE else id.fun()

    if (!identified) {
        output <- do.call(FUN, args)
        fail_code <- ifelse(allow.redirect, 400L, 300L)
        if (output$status_code >= fail_code && !is.null(head.fun)) {
            # If that didn't work, well, we'll just have to try to get some headers.
            # This only works if a header function is available, of course.
            identified <- TRUE
        }
    }

    # Separate if/else to catch situations where identified is recently set to TRUE. 
    if (identified) {
        if (!is.null(head.fun)) {
            args <- c(args, list(do.call(add_headers, head.fun())))
        }
        output <- do.call(FUN, args)
    }

    output
}

#' @export
#' @rdname authorizedVerb
#' @importFrom httr content http_status
checkResponse <- function(x, allow.redirect=FALSE) {
    fail_code <- ifelse(allow.redirect, 400L, 300L)
    if (x$status_code >= fail_code) {
        contents <- content(x)
        msg <- http_status(x$status_code)$message
        if (is.list(contents) && identical(contents$status, "error") && !is.null(contents$reason)) {
            msg <- paste0(msg, "\n  ", contents$reason)
        }
        stop(msg)
    }
}

auth.env <- new.env()
auth.env$available <- NULL
auth.env$headers <- NULL

#' @export
#' @rdname authorizedVerb
identityAvailable <- function(fun) {
    prev <- auth.env$available
    if (!missing(fun)) {
        auth.env$available <- fun
        invisible(prev)
    } else {
        prev
    }
}

#' @export
#' @rdname authorizedVerb
identityHeaders <- function(fun) {
    prev <- auth.env$headers
    if (!missing(fun)) {
        auth.env$headers <- fun
        invisible(prev)
    } else {
        prev
    }
}
