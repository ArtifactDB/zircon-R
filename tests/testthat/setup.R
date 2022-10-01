setGithubIdentities <- function() {
    token <- Sys.getenv("GITHUB_TOKEN", NA)
    if (is.na(token)) {
        return(NULL)
    }

    olda <- identityAvailable(function() TRUE)
    oldh <- identityHeaders(function() list(Authorization=paste0("Bearer ", token)))

    function() {
        identityAvailable(olda)
        identityHeaders(oldh)
    }
}

# Defining a cache.
tmp.cache <- file.path(tempdir(), "zircon-cache")
dir.create(tmp.cache, showWarnings=FALSE)
cachedCounter <- new.env()
cachedCounter$hits <- 0L

cacheTemporary <- function(key, save) {
    path <- file.path(tmp.cache, URLencode(key, reserved=TRUE, repeated=TRUE))
    if (!file.exists(path)) {
        save(path)
    } else {
        cachedCounter$hits <- cachedCounter$hits + 1L
    }
    path
}
