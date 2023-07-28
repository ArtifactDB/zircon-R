#' Authenticating via GitHub 
#'
#' Configure \code{\link{authorizedVerb}} to use GitHub personal access tokens for authentication.
#' Each GitHub token is then converted into a JSON web token for use in the ArtifactDB API.
#' This is a useful default approach when no better authentication scheme is available.
#'
#' @param github.token String containing a GitHub personal access token.
#' This should have the \code{"read:org"} and \code{"read:user"} scopes.
#' If missing, the user will be prompted to use GitHub's Oauth web application flow to acquire a token.
#' If \code{NULL}, any existing tokens are cleared.
#' @param cache.env Environment to use to store the current GitHub token in an in-memory cache.
#' This allows the same token to be transparently re-used in the same R session.
#' @param github.cache.path String containing a path to a file in which to store the GitHub token.
#' This allows the same token to be transparently re-used across different R sessions.
#' If \code{NULL}, no on-disk caching is performed.
#' @param github.app.key String containing the key for a GitHub Oauth app.
#' If \code{NULL}, the default \dQuote{ArtifactDB} application is used.
#' @param github.app.secret String containing the secret for a GitHub Oauth app.
#' If \code{NULL}, the default \dQuote{ArtifactDB} application is used.
#' @param jwt.cache.path String containing a path to a file in which to store the JSON web token.
#' This allows the same token to be transparently re-used across different R sessions.
#' If \code{NULL}, no on-disk caching is performed.
#' @param prompt Logical scalar indicating whether the user should be prompted to supply token details if no cached token exists.
#' @param github.url String containing the URL to a GitHub REST API.
#' @param jwt.url String containing the URL to a API to convert the GitHub token into a JSON web token.
#' @param user.agent String specifying the user agent for queries to various endpoints.
#' @param org.id String containing the GitHub organization to use for identifying ArtifactDB roles.
#' Users are expected to have public membership in this organization.
#' @param ... For \code{getGitHubTokenInfo}, further arguments to pass to \code{setGitHubToken}.
#' 
#' For \code{getJWTFromGitHub}, further arguments to pass to \code{getGitHubTokenInfo}. 
#'
#' For \code{useGitHubIdentities}, further arguments to pass to \code{getJWTFromGitHub}.
#'
#' @return 
#' For \code{setGitHubToken}, any non-\code{NULL} \code{github.token} (or its interactively supplied counterpart) will be cached in memory and/or on disk, depending on \code{cache.path}.
#' A list is invisibly returned containing details about the GitHub token including its value, the corresponding user account and the expiry time.
#' If \code{token=NULL}, any cached token is cleared from disk and memory, and \code{NULL} is invisibly returned.
#'
#' For \code{getGitHubTokenInfo}, a list is returned containing the GitHub token details.
#' If \code{prompt=FALSE} and no cached token is present, \code{NULL} is returned.
#'
#' For \code{getGitHubTokenInfo}, a list is returned containing the JSON web token and the expiry time.
#' If no JSON web token is available but a GitHub personal access token is available, the latter will be used to request the former via \code{jwt.url}.
#' If \code{prompt=FALSE} and no cached GitHub token is present, \code{NULL} is returned.
#'
#' For \code{useGitHubIdentities}, the availability and value of the JSON web token is used to configure \code{\link{identityAvailable}} and \code{\link{identityHeaders}}.
#' A function is returned that, when executed, restores the previous values of \code{\link{identityAvailable}} and \code{\link{identityHeaders}};
#' this is typically run in an \code{\link{on.exit}} clause.
#'
#' @details
#' We use GitHub as an identity provider for authenticating users, under the assumption that most users will have a GitHub account.
#' Users can log in via the usual Oauth web application flow to obtain a GitHub access token.
#' This is, in turn, converted into a JSON web token with roles that can be used to authorize actions on an ArtifactDB instance.
#'
#' We use GitHub teams to define roles for an ArtifactDB instance.
#' Specifically, for a given organization in \code{org.id}, we look for any of the following teams:
#' \itemize{
#' \item \code{ArtifactDB-admins}: Users with administrative privileges in the ArtifactDB instance.
#' This corresponds to an \code{"admin"} role.
#' \item \code{ArtifactDB-creators}: Users who can create new projects.
#' This corresponds to a \code{"creator"} role.
#' \item \code{ArtifactDB-uploaders}: Users who can upload to existing projects.
#' This corresponds to an \code{"uploader"} role.
#' }
#' Maintainers of an ArtifactDB instance should GitHub users to these teams to give them the appropriate roles.
#'
#' @author Aaron Lun
#'
#' @examples
#' cache.env <- new.env()
#' gh.cache.path <- tempfile()
#' \dontrun{setGitHubToken(cache.env=cache.env, github.cache.path=gh.cache.path)}
#'
#' getGitHubTokenInfo(cache.env, github.cache.path=gh.cache.path)
#'
#' jwt.cache.path <- tempfile()
#' getJWTFromGitHub(cache.env, 
#'    "CollaboratorDB", 
#'    jwt.cache.path=jwt.cache.path, 
#'    github.cache.path=gh.cache.path)
#'
#' @export
#' @rdname github
#' @importFrom httr GET add_headers headers content oauth_app oauth2.0_token oauth_endpoints
setGitHubToken <- function(github.token, cache.env, github.cache.path=NULL, github.url="https://api.github.com", github.app.key = NULL, github.app.secret = NULL, user.agent=NULL) {
    do.cache <- !is.null(github.cache.path)

    if (!missing(github.token) && is.null(github.token)) {
        cache.env$auth.info <- NULL
        if (do.cache) { # don't write to disk if cache=FALSE.
            unlink(github.cache.path)
        }
        return(invisible(NULL))
    }

    if (missing(github.token)) {
        if (is.null(github.app.key) || is.null(github.app.secret)) {
            res <- GET(paste0(default.laundry, "/.github-app-info"), .user_agent(user.agent))
            if (res$status_code >= 300) {
                stop("failed to fetch GitHub Oauth application key and secret (status code ", res$status_code, ")")
            }
            info <- content(res)
            github.app.key <- info$key
            github.app.secret <- info$secret
        }
        oapp <- oauth_app("github", key = github.app.key, secret = github.app.secret)
        ores <- oauth2.0_token(oauth_endpoints("github"), oapp, scope=c("read:org", "read:user"), cache=FALSE)
        github.token <- ores$credentials$access_token
    }

    endpoint <- paste0(github.url, "/user")
    res <- GET(endpoint, add_headers(Authorization=paste("Bearer ", github.token)), .user_agent(user.agent))
    if (res$status_code >= 300) {
        stop("failed to verify this token with GitHub (status code ", res$status_code, ")")
    }
    name <- content(res)$login
    expiry <- Inf
    expires <- headers(res)[["github-authentication-token-expiration"]]
    if (!is.null(expires)) {
        frags <- strsplit(expires, " ")[[1]]
        expiry <- as.double(as.POSIXct(paste(frags[1], frags[2]), tz=frags[3]))
    }

    if (do.cache) {
        dir.create(dirname(github.cache.path), showWarnings=FALSE, recursive=TRUE)
        writeLines(c(github.token, name, expiry), con=github.cache.path)
    }
    vals <- list(token=github.token, name=name, expires=expiry)
    cache.env$auth.info <- vals
    invisible(vals)
}

default.laundry <- "https://gh2jwt.aaron-lun.workers.dev"

#' @export
#' @rdname github
getGitHubTokenInfo <- function(cache.env, github.cache.path=NULL, prompt=interactive(), ...) {
    vals <- cache.env$auth.info

    rerun <- FALSE
    if (is.null(vals)) {
        if (is.null(github.cache.path) || !file.exists(github.cache.path)) {
            rerun <- TRUE
        } else {
            lines <- readLines(github.cache.path)
            vals <- list(token = lines[1], name = lines[2], expires = as.double(lines[3]))
            if (vals$expires <= as.double(Sys.time())) {
                unlink(github.cache.path)
                cache.env$auth.info <- NULL
                rerun <- TRUE
            }
        }
    } else {
        if (vals$expires <= as.double(Sys.time())) {
            if (!is.null(github.cache.path)) {
                unlink(github.cache.path)
            }
            cache.env$auth.info <- NULL
            rerun <- TRUE
        }
    }

    if (rerun) {
        if (prompt) {
            vals <- setGitHubToken(cache.env=cache.env, github.cache.path=github.cache.path, ...)
        } else {
            return(NULL)
        }
    }

    vals
}

#' @export
#' @rdname github
#' @importFrom httr POST add_headers content
getJWTFromGitHub <- function(cache.env, org.id, jwt.url=NULL, jwt.cache.path=NULL, user.agent=NULL, ...) {
    token.info <- cache.env$jwt
    jwt.cache <- !is.null(jwt.cache.path)

    rerun <- FALSE
    if (is.null(token.info)) {
        if (!jwt.cache || !file.exists(jwt.cache.path)) {
            rerun <- TRUE
        } else {
            raw.token.info <- readLines(jwt.cache.path)
            token.info <- list(token = raw.token.info[1], expires = as.double(raw.token.info[2]))
            if (token.info$expires <= as.double(Sys.time())) {
                unlink(jwt.cache.path)
                cache.env$jwt <- NULL
                rerun <- TRUE
            }
        }
    } else {
        if (token.info$expires <= as.double(Sys.time())) {
            if (!jwt.cache) {
                unlink(jwt.cache.path)
            }
            cache.env$jwt <- NULL
            rerun <- TRUE
        }
    }

    if (rerun) {
        gh <- getGitHubTokenInfo(cache.env=cache.env, user.agent=user.agent, ...)
        if (is.null(gh)) {
            return(NULL)
        }

        if (is.null(jwt.url)) {
            jwt.url <- default.laundry
        }
        endpoint <- paste0(jwt.url, "/token")
        res <- POST(endpoint, body = list(orgs = I(org.id), to = org.id), encode = "json", add_headers(Authorization = paste("Bearer ", gh$token)), .user_agent(user.agent))
        checkResponse(res)

        info <- content(res)
        token.info <- list(token = info$token, expires = as.double(as.POSIXct(info$expires_at, format="%Y-%m-%dT%H:%M:%OSZ")))

        if (jwt.cache) {
            dir.create(dirname(jwt.cache.path), showWarnings=FALSE, recursive=TRUE)
            writeLines(c(token.info$token, token.info$expires), con=jwt.cache.path)
        }
        cache.env$jwt <- token.info
    }

    token.info
}

#' @export
#' @rdname github
useGitHubIdentities <- function(cache.env, jwt.cache.path=NULL, gh.cache.path=NULL, ...) {
    olda <- identityAvailable(function() 
        !is.null(getJWTFromGitHub(cache.env=cache.env, cache.path=gh.cache.path, prompt=FALSE, ...))
    )

    oldh <- identityHeaders(function() 
        list(Authorization=paste0("Bearer ", getJWTFromGitHub(cache.env=cache.env, jwt.cache.path=jwt.cache.path, gh.cache.path=gh.cache.path, ...)$token))
    )

    function() {
        identityAvailable(olda)
        identityHeaders(oldh)
    }
}
