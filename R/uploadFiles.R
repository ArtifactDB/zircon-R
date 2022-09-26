#' Upload utilities, for wizards only
#'
#' These are utilities for uploading files to ArtifactDB.
#' Not for the end-user or the typical package developer.
#'
#' @param url String containing the URL to the REST API.
#' @param token String containing a Keycloak token to authenticate the uploader.
#' @param index.wait Numeric scalar specifying the number of seconds to wait when checking for correct indexing.
#' @param must.index Logical scalar indicating whether to throw an error if the indexing is not successful.
#' If \code{FALSE}, a warning is raised if the indexing times out (but an error will still be raised if the indexing explicitly fails).
#' 
#' @return
#' \code{initializeUpload} will return the \code{response} object from hitting the upload endpoint.
#' This will have already been checked for failure.
#' 
#' \code{uploadFiles} will upload all files to their URLs and return \code{NULL}.
#'
#' \code{requestCompletion} will return the \code{response} object from hitting the completion endpoint.
#' This will have already been checked for failure.
#'
#' \code{abortUrl} will return the \code{response} object from hitting the abort endpoint.
#' This will \emph{not} have been checked for failure, not least because this function is typically called in response to other errors during the upload;
#' we leave it to the caller to decide whether to add another error to the trace.
#'
#' @author Aaron Lun
#' @export
#' @rdname upload-utils
#' @importFrom httr POST add_headers 
initializeUpload <- function(dir, files, start.url, dedup.md5=NULL, dedup.md5.field=NULL, dedup.link=NULL, expires=NULL, user.agent=NULL) {
    stopifnot(length(intersect(files, names(dedup.link)))==0L)
    stopifnot(length(intersect(files, names(dedup.md5)))==0L)
    stopifnot(length(intersect(names(dedup.link), names(dedup.md5)))==0L)

    files <- as.list(files)
    for (f in files) {
        .check_file_size(dir, f)
    }

    md5.files <- vector("list", length(dedup.md5))
    for (i in seq_along(md5.files)) {
        stopifnot(!is.null(dedup.md5.field))
        fname <- names(dedup.md5)[i]
        .check_file_size(dir, fname)
        md5.files[[i]] <- list(filename=fname, check="md5", value=list(field=dedup.md5.field, md5sum=dedup.md5[[i]]))
    }

    link.files <- vector("list", length(dedup.link))
    for (i in seq_along(link.files)) {
        # No need to check the file size here, as we're not actually uploading it.
        link.files[[i]] <- list(filename=names(dedup.link)[i], check="link", value=list(artifactdb_id=dedup.link[[i]]))
    }

    body <- list(
        filenames = c(files, md5.files, link.files),
        mode = "s3-presigned-url"
    )
    if (!is.null(expires)) {
        body$expires_in <- paste("in", expires, "days")
        body$completed_by <- body$expires_in # this needs to be <= expiry date.
    }

    added <- .follow_redirects_faithfully(POST, start.url, body=body, encode='json', user.agent=user.agent)
    checkResponse(added)

    added
}

#' @export
#' @rdname upload-utils
createUploadStartUrl <- function(url, project, version) {
    paste0(url, "/projects/", project, "/version/", version, "/upload")
}

.check_file_size <- function(dir, f) {
    full <- file.path(dir, f)
    fsize <- file.info(full)$size
    gig <- 2^30
    limit <- min(5, getOption("ArtifactDB.upload.size.limit", 1))
    if (fsize >= limit * gig) {
        stop(paste(strwrap(paste0("refusing to upload '", full, "', which is over ", limit, " GB in size - see ?.uploadFiles for details"), 80), collapse="\n"))
    }
}

#' @export
#' @rdname upload-utils
#' @importFrom httr PUT stop_for_status
uploadFiles <- function(dir, url, initial, user.agent=NULL, attempts=3) {
    dedup.urls <- initial$links
    for (d in names(dedup.urls)) {
        current <- dedup.urls[[d]]
        if (!startsWith(current, "http")) {
            current <- paste0(url, "/", current)
        }
        out <- .follow_redirects_faithfully(PUT, current, user.agent=user.agent)
        checkResponse(out)
    } 

    # Looping through all files and uploading them. Each upload undergoes several 
    # attempts to be robust to connection loss or timeouts.
    up.urls <- initial$presigned_urls
    for (g in names(up.urls)) {
        up.url <- up.urls[[g]]
        failed <- TRUE

        if (!is.null(up.url)) {
            for (i in seq_len(attempts)) {
                # Why doesn't httr::PUT work? I DON'T KNOW!
                out <- curl::curl_upload(file.path(dir, g), up.url, useragent=.raw_user_agent(), verbose=FALSE)
                if (out$status_code < 300) {
                    failed <- FALSE
                    break
                }
                # Try again after some time, maybe it's feeling better.
                Sys.sleep(60)
            }
        }

        if (failed) {
            stop("failed to upload ", g)
        } 
    }
}

#' @export
#' @rdname upload-utils
#' @importFrom httr PUT add_headers 
completeUpload <- function(url, initial, index.wait=600, must.index=FALSE, permissions=list(), overwrite.permissions=FALSE, user.agent=NULL) {
    end.url <- paste0(url, initial$completion_url)

    if (overwrite.permissions) {
        end.url <- paste0(end.url, "&overwrite_permissions=true")
    }

    if (is.null(names(permissions))){
        names(permissions) <- seq_along(permissions)
    }
    if (!is.null(permissions$viewers)) {
        permissions$viewers <- I(permissions$viewers)
    }
    if (!is.null(permissions$owners)) {
        permissions$owners <- I(permissions$owners)
    }

    if (is.null(end.url)) {
        end.url <- initial$completion_url
    }

    fin <- .follow_redirects_faithfully(PUT, end.url, body=permissions, encode="json", user.agent=user.agent)
    checkResponse(fin)

    guts <- content(fin)
    status.url <- paste0(url, "/jobs/", guts$job_id)
    okay <- failed <- FALSE 

    counter <- 5 
    while (index.wait > 0) {
        Sys.sleep(counter)
        index.wait <- index.wait - counter
        X <- GET(status.url)
        if (X$status_code < 300) {
            index.info <- content(X)
            index.status <- index.info$status
            if (index.status == "SUCCESS") {
                okay <- TRUE
                break
            } else if (index.status == "FAILURE") {
                failed <- TRUE
                break
            }
        }
    }

    if (!okay) {
        if (failed || must.index) {
            stop(paste(strwrap(paste0("indexing failure for job '", guts$job_id, "', see ", status.url, " for more details"), 80), collapse="\n  "))
        } else {
            warning(paste(strwrap(paste0("indexing timeout for job '", guts$job_id, "', see ", status.url, " for more details"), 80), collapse="\n  "))
        }
    }

    fin
}

#' @export
#' @rdname upload-utils
#' @importFrom httr PUT
abortUpload <- function(url, initial, user.agent=NULL) {
    abort.url <- paste0(url, initial$abort_url)
    .follow_redirects_faithfully(PUT, abort.url, user.agent=user.agent)
}
