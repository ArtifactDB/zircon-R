#' Authenticating via GitHub 
#'
#' Configure \code{\link{authorizedVerb}} to use GitHub personal access tokens for authentication.
#' This is a useful default approach when no better authentication scheme is available.
#'
#' @param token String containing a GitHub personal access token.
#' If missing, the user will be prompted to supply a token.
#' If \code{NULL}, any existing tokens are cleared.
#' @param cache.env Environment to use to store the current GitHub token in an in-memory cache.
#' This allows the same token to be transparently re-used in the same R session.
#' @param cache.path String containing a path to a on-disk cache.
#' This allows the same token to be transparently re-used across different R sessions.
#' If \code{NULL}, no on-disk caching is performed.
#' @param prompt.text String containing the prompt text for an interactively supplied token.
#' @param prompt Logical scalar indicating whether the user should be prompted to supply token details if no cached token exists.
#' @param github.url String containing the URL to a GitHub REST API.
#' @param user.agent String specifying the user agent for queries to various endpoints.
#' @param ... Further arguments to pass to \code{setGitHubToken} when prompting for a new token.
#'
#' @return 
#' For \code{setGitHubToken}, any non-\code{NULL} \code{token} (or its interactively supplied counterpart) will be cached in memory and/or on disk, depending on \code{cache.path}.
#' A list is invisibly returned containing details about the token including its value, the corresponding user account and the expiry time.
#' If \code{token=NULL}, any cached token is cleared from disk and memory, and \code{NULL} is invisibly returned.
#'
#' For \code{getGitHubTokenInfo}, a list is returned containing the token details.
#' If \code{prompt=FALSE} and no cached token is present, \code{NULL} is returned.
#'
#' For \code{useGitHubIdentities}, the availability and value of the token is used to configure \code{\link{identityAvailable}} and \code{\link{identityHeaders}}.
#' A function is returned that, when executed, restores the previous values of \code{\link{identityAvailable}} and \code{\link{identityHeaders}};
#' this is typically run in an \code{\link{on.exit}} clause.
#'
#' @author Aaron Lun
#'
#' @examples
#' \dontrun{setGitHubToken()}
#'
#' cache.env <- new.env()
#' cache.path <- tempfile()
#' getGitHubTokenInfo(cache.env, cache.path, prompt=FALSE)
#'
#' @export
#' @rdname github
#' @importFrom httr GET add_headers content
setGitHubToken <- function(token, cache.env, cache.path=NULL, github.url="https://api.github.com", user.agent=NULL, prompt.text=NULL) {
    cache <- !is.null(cache.path)

    if (!missing(token) && is.null(token)) {
        cache.env$auth.info <- NULL
        if (cache) { # don't write to disk if cache=FALSE.
            unlink(cache.path)
        }
        return(invisible(NULL))
    }

    expiry <- NULL
    name <- NULL
    endpoint <- paste0(github.url, "/user")

    if (missing(token)) {
        if (is.null(prompt.text)) {
            prompt.text <- "Please generate a new Github personal access token.

1. Go to https://github.com/settings/tokens.
2. Click 'Generate new token' and then 'Generate new token (classic)'.
3. Give it a name and (optionally) set the desired expiry time.
4. Make sure the 'read:org' box is checked.
5. Click 'Generate token'.
6. Copy and paste the token string into the prompt below."
        }

        cat(prompt.text)
        token <- readline("\n\nTOKEN: ")
        while (nchar(token)) {
            res <- GET(endpoint, add_headers(Authorization=paste("Bearer ", token)), .user_agent(user.agent))
            if (res$status_code < 300) {
                expiry <- .process_expiry(res)
                name <- content(res)$login
                break
            }

            more.prompt <- paste0("\nHmm... failed to verify this token with GitHub (status code ", res$status_code, "). Try again?\nTOKEN: ")
            token <- readline(more.prompt)
        }

        if (nchar(token) == 0) {
            stop("empty token supplied")
        }

    } else if (!is.null(token)) {
        res <- GET(endpoint, add_headers(Authorization=paste("Bearer ", token)), .user_agent(user.agent))
        if (res$status_code >= 300) {
            stop("failed to verify this token with GitHub (status code ", res$status_code, ")")
        }
        expiry <- .process_expiry(res)
        name <- content(res)$login
    }

    if (cache) {
        dir.create(dirname(cache.path), showWarnings=FALSE, recursive=TRUE)
        writeLines(c(token, name, expiry), con=cache.path)
    }
    vals <- list(token=token, name=name, expires=expiry)
    cache.env$auth.info <- vals
    invisible(vals)
}

#' @export
#' @rdname github
getGitHubTokenInfo <- function(cache.env, cache.path=NULL, prompt=interactive(), ...) {
    vals <- cache.env$auth.info

    rerun <- FALSE
    if (is.null(vals)) {
        if (is.null(cache.path) || !file.exists(cache.path)) {
            rerun <- TRUE
        } else {
            lines <- readLines(cache.path)
            vals <- list(token = lines[1], name = lines[2], expires = as.double(lines[3]))
            if (vals$expires <= as.double(Sys.time())) {
                unlink(cache.path)
                cache.env$auth.info <- NULL
                rerun <- TRUE
            }
        }
    } else {
        if (vals$expires <= as.double(Sys.time())) {
            if (!is.null(cache.path)) {
                unlink(cache.path)
            }
            cache.env$auth.info <- NULL
            rerun <- TRUE
        }
    }

    if (rerun) {
        if (prompt) {
            vals <- setGitHubToken(cache.env=cache.env, cache.path=cache.path, ...)
        } else {
            return(NULL)
        }
    }

    vals
}

#' @importFrom httr headers
.process_expiry <- function(res) {
    expires <- headers(res)[["github-authentication-token-expiration"]]
    if (!is.null(expires)) {
        frags <- strsplit(expires, " ")[[1]]
        as.double(as.POSIXct(paste(frags[1], frags[2]), tz=frags[3]))
    } else {
        Inf
    }
}

#' @export
#' @rdname github
#' @importFrom httr POST add_headers content
getJWTFromGitHub <- function(cache.env, client.id, jwt.cache.path=NULL, gh.cache.path=NULL, prompt=interactive(), user.agent=NULL, ...) {
    token.info <- cache.env$jwt
    jwt.cache <- !is.null(jwt.cache.path)

    rerun <- FALSE
    if (is.null(token.info)) {
        if (!jwt.cache || !file.exists(jwt.cache.path)) {
            rerun <- TRUE
        } else {
            raw.token.info <- readLines(jwt.cache.path)
            token.info <- list(token = raw.token.info[1], expiry = as.double(raw.token.info[2]))
            if (token.info$expiry <= as.double(Sys.time())) {
                unlink(jwt.cache.path)
                cache.env$jwt <- NULL
                rerun <- TRUE
            }
        }
    } else {
        if (token.info$expiry <= as.double(Sys.time())) {
            if (!jwt.cache) {
                unlink(jwt.cache.path)
            }
            cache.env$jwt <- NULL
            rerun <- TRUE
        }
    }

    if (rerun) {
        gh <- getGitHubTokenInfo(cache.env=cache.env, cache.path=gh.cache.path, prompt=prompt, user.agent=user.agent, ...)
        if (is.null(gh)) {
            return(NULL)
        }

        laundry <- "https://gh2jwt.aaron-lun.workers.dev/token"
        res <- POST(laundry, body = list(orgs = I(client.id), to = client.id), encode = "json", add_headers(Authorization = paste("Bearer ", gh$token)), .user_agent(user.agent))
        checkResponse(res)

        info <- content(res)
        token.info <- list(token = info$token, expiry = as.double(as.POSIXct(info$expires_at, format="%Y-%m-%dT%H:%M:%OSZ")))
        if (jwt.cache) {
            dir.create(dirname(jwt.cache.path), showWarnings=FALSE, recursive=TRUE)
            writeLines(c(token.info$token, token.info$expiry), con=jwt.cache.path)
        }
    }

    token.info
}

#' @export
#' @rdname github
useGitHubIdentities <- function(cache.env, jwt.cache.path=NULL, gh.cache.path=NULL, ...) {
    olda <- identityAvailable(function() !is.null(getGitHubTokenInfo(cache.env=cache.env, cache.path=gh.cache.path, prompt=FALSE, ...)))
    oldh <- identityHeaders(function() list(Authorization=paste0("Bearer ", getJWTFromGitHub(cache.env=cache.env, jwt.cache.path=jwt.cache.path, gh.cache.path=gh.cache.path, ...)$token)))
    function() {
        identityAvailable(olda)
        identityHeaders(oldh)
    }
}
