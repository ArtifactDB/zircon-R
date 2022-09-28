#' @importFrom utils packageVersion
.raw_user_agent <- function() {
    sinfo <- Sys.info()            
    sprintf("zircon version %s; %s; %s release %s", 
        packageVersion("zircon"), R.version.string,
        sinfo[["sysname"]], sinfo[["release"]])
}

#' @importFrom httr user_agent
.user_agent <- function(agent=NULL) {
    if (is.null(agent)) {
        agent <- .raw_user_agent()
    }
    user_agent(agent)
}

#' @importFrom utils head
.resolve_redirect <- function(old.url, new.url) {
    if (!startsWith(new.url, "http")) {
        fragments <- strsplit(old.url, "/")[[1]]
        if (startsWith(new.url, "/")) {
            domain <- paste(fragments[1:3], collapse="/")
            new.url <- paste0(domain, new.url)
        } else {
            host <- paste(head(fragments, -1), collapse="/")
            new.url <- paste0(host, "/", new.url)
        }
    }
    new.url
}

#' @importFrom httr config
.follow_redirects_faithfully <- function(FUN, url, ...) {
    attempts <- character(0)
    repeat {
        req <- authorizedVerb(FUN, url, ..., config(followlocation = 0L))
        if (req$status_code >= 300 && req$status_code < 400) {
            attempts <- c(attempts, url)

            # Figuring out whether this is a relative path or not.
            new.url <- req$headers$location
            new.url <- .resolve_redirect(url, new.url)

            if (new.url %in% attempts) {
                stop("recursive redirection to '", new.url, "'")
            }
            url <- new.url
            next
        }
        return(req)
    }
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
