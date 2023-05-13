#' Search on metadata
#'
#' Search an ArtfiactDB based on the file/project metadata.
#' 
#' @param q String containing an Elasticsearch-compatible query.
#' @param fields Character vector containing the fields to return in the metadata for each matching file.
#' If \code{NULL}, all fields are returned.
#' @param url String containing the URL of the ArtifactDB REST endpoint.
#' @param number Integer scalar specifying the maximum number of records to return.
#' Defaults to 50.
#' @param latest Logical scalar indicating whether to only return files for the latest revision of each project.
#' @param warn.auth Logical scalar indicating whether to warn the user if they are not authenticated.
#' @param user.agent String specifying the user agent, defaults to the current R installation and \pkg{zircon} version.
#' @param transient Logical scalar indicating whether to return transient files, i.e., with an expiry date.
#' @param quote.dash Logical scalar indicating whether words containing dashes should quoted to avoid tokenization of the components.
#' 
#' @return A list of lists where each inner list corresponds to a matching file and contains its metadata.
#' The metadata entry for each file is equivalent to that returned by \code{\link{.getFileMetadata}}.
#'
#' @details
#' If \code{latest=TRUE} and \code{fields} is not \code{NULL}, the output list will contain extra fields required for sorting versions.
#' These have no effect on the other returned values and can be safely ignored if they are not of interest.
#'
#' By default, if the user is not authenticated, the function will only return public results with a warning.
#' The warning can be disabled with \code{warn=FALSE} if the public restriction is intentional.c
#'
#' If \code{number=NULL}, a warning is raised if the search results are truncated.
#'
#' @author Aaron Lun
#'
#' @examples
#' searchMetadata(example.project, field=c("path", "description"), url=example.url)
#'
#' @export
#' @importFrom httr GET content headers
#' @importFrom utils head URLencode
searchMetadata <- function(q, url, fields=NULL, number=NULL, latest=TRUE, warn.auth=TRUE, transient=FALSE, quote.dash=TRUE, user.agent=NULL) {
    if (quote.dash) {
        q <- .inject_dashes(q)
    }

    URL <- file.path(url, paste0("search?q=", URLencode(q, reserved=TRUE)))
    if (!is.null(fields)) {
        if (!transient) {
            fields <- union(fields, "_extra.transient")
        }
        URL <- paste0(URL, "&fields=", paste(fields, collapse=","))
    }

    if (latest) {
        URL <- paste0(URL, "&latest=true")
    }

    if (warn.trunc <- is.null(number)) {
        number <- 50
    }

    collected <- list()
    mostrecent <- list()

    idfun <- identityAvailable()
    if (!is.null(idfun)) {
        if (!idfun() && warn.auth) {
            warning("only showing public search results for non-authenticated user, use gp.auth::login() to authenticate")
        }
    }

    repeat {
        sout <- authorizedVerb(FUN=GET, url=URL, user.agent=user.agent)
        checkResponse(sout)

        payload <- content(sout, simplifyVector=TRUE, simplifyDataFrame=FALSE, simplifyMatrix=FALSE)
        harvest <- payload$results
        if (length(harvest)==0) {
            break
        }

        if (!transient) {
            is.trans <- vapply(harvest, function(x) is.list(x$`_extra`$transient), TRUE)
            harvest <- harvest[!is.trans]
        }

        collected <- c(collected, harvest)
        if (length(collected) > number) {
            if (warn.trunc) {
                warning("search results truncated to the first ", number, " entries")
            }
            break
        } 

        link.info <- headers(sout)$link
        URL <- .parse_link_url(url, link.info)
        if (is.null(URL)) {
            break
        }
    }

    head(collected, number)
}

.parse_link_url <- function(url, link.info) {
    if (is.null(link.info)) {
        return(NULL)
    }

    all.links <- strsplit(link.info, split=", ")[[1]]
    chosen <- grep('rel=more', all.links)
    if (length(chosen)!=1L) {
        return(NULL) 
    } 

    raw.url <- all.links[chosen]
    paste0(url, sub("<(.*)>.*", "\\1", all.links[chosen]))
}

.inject_dashes <- function(q) {
    collected <- character()
    sofar <- 1L
    inquote <- FALSE

    # Allow multi-dash words.
    part_of_word <- function(x) {
        return (grepl("[a-zA-Z0-9]", x) || x == "-")
    }

    i <- 1L
    while (i <= nchar(q)) {
        current <- substr(q, i, i)
        if (current == "\"") {
            inquote <- !inquote
        } else if (!inquote) {
            if (current == "-") {
                preceding <- i 
                while (preceding > 1L) {
                    if (!part_of_word(substr(q, preceding - 1L, preceding - 1L))) {
                        break
                    }
                    preceding <- preceding - 1L
                }

                following <- i
                while (following < nchar(q)) {
                    if (!part_of_word(substr(q, following + 1L, following + 1L))) {
                        break
                    }
                    following <- following + 1L
                }

                if (preceding != i && following != i) {
                    collected <- c(collected, substr(q, sofar, preceding - 1L), "\"", substr(q, preceding, following), "\"")
                    sofar <- following + 1L
                    i <- following
                }
            }
        }
        i <- i + 1L
    } 

    collected <- c(collected, substr(q, sofar, nchar(q)))
    paste(collected, collapse="")
}
