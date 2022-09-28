# This tests the upload machinery based on the private key.
# library(testthat); library(zircon); source("setup-private.R"); source("test-upload.R")

token <- Sys.getenv("GITHUB_TOKEN", NA)
if (is.na(token)) {
    skip("missing a GITHUB_TOKEN environment variable for uploads")
}
olda <- identityAvailable(function() TRUE)
oldh <- identityHeaders(function() list(Authorization=paste0("Bearer ", token)))

basic <- list(
    `$schema`="generic_object/v1.json",
    annotated_object = list(
        language="R",
        class="letters"
    )
)

tmp <- tempfile()
dir.create(tmp)

rpath1 <- "whee.rds"
rpath10 <- paste0(rpath1, ".json")
fpath1 <- file.path(tmp, rpath1)
saveRDS(file=fpath1, letters)
fpath10 <- file.path(tmp, rpath10)
md5.1 <- digest::digest(file=fpath1)
write(file=fpath10, jsonlite::toJSON(c(basic, list(md5sum=md5.1, path=basename(fpath1))), auto_unbox=TRUE, pretty=TRUE))

rpath2 <- "blah.rds"
rpath20 <- paste0(rpath2, ".json")
fpath2 <- file.path(tmp, rpath2)
saveRDS(file=fpath2, LETTERS)
fpath20 <- file.path(tmp, rpath20)
md5.2 <- digest::digest(file=fpath2)
write(file=fpath20, jsonlite::toJSON(c(basic, list(md5sum=md5.2, path=basename(fpath2))), auto_unbox=TRUE, pretty=TRUE))

rpath3 <- "foo/bar.rds"
rpath30 <- paste0(rpath3, ".json")
dir.create(file.path(tmp, "foo"))
fpath3 <- file.path(tmp, rpath3)
saveRDS(file=fpath3, 1:100)
md5.3 <- digest::digest(file=fpath3)
fpath30 <- file.path(tmp, rpath30)
write(file=fpath30, jsonlite::toJSON(c(basic, list(md5sum=md5.3, path=basename(fpath3))), auto_unbox=TRUE, pretty=TRUE))

### Base persistent upload, for reliable testing.
# f <- list.files(tmp, recursive=TRUE)
# start.url <- createUploadStartUrl(example.url, "test-zircon-upload", "base")
# info <- initializeUpload(tmp, f, start.url)
# parsed <- httr::content(info)
# uploadFiles(tmp, example.url, parsed)
# completeUpload(example.url, parsed)

first_version <- as.integer(Sys.time())
test_that("basic upload sequence works correctly", {
    f <- list.files(tmp, recursive=TRUE)

    start.url <- createUploadStartUrl(example.url, "test-zircon-upload", first_version)
    info <- initializeUpload(tmp, f, start.url, expires=1)

    parsed <- httr::content(info)
    expect_true(all(f %in% names(parsed$presigned_urls)))

    uploadFiles(tmp, example.url, parsed)
    comp <- completeUpload(example.url, parsed)

    res <- getFileMetadata(paste0("test-zircon-upload:blah.rds@", first_version), url=example.url)
    expect_identical(res$path, "blah.rds")

    contents <- getFile(paste0("test-zircon-upload:blah.rds@", first_version), url=example.url)
    expect_identical(readRDS(contents), LETTERS)
})

create_md5_links <- function(dir, to.link) {
    mlinks <- list()
    for (x in to.link) {
        mlinks[[x]] <- jsonlite::fromJSON(file.path(dir, paste0(x, ".json")))$md5sum
    }
    return(mlinks)
}

test_that("md5-linked uploads work correctly (valid)", {
    # Creating another version now. This assumes that we can piggy-back off the base version.
    Sys.sleep(1)
    version <- as.integer(Sys.time()) 

    f <- list.files(tmp, recursive=TRUE)
    linkable <- which(!grepl(".json$", f))
    mlinks <- create_md5_links(tmp, f[linkable])
    expect_true(length(mlinks) > 1)
    remaining <- f[-linkable]

    start.url <- createUploadStartUrl(example.url, "test-zircon-upload", version)
    info <- initializeUpload(tmp, remaining, start.url, dedup.md5=mlinks, dedup.md5.field="md5sum", expires=1)

    parsed <- httr::content(info)
    expect_true(all(f[linkable] %in% names(parsed$links)))
    expect_true(all(remaining %in% names(parsed$presigned_urls)))

    uploadFiles(tmp, example.url, parsed)
    comp <- completeUpload(example.url, parsed)

    # Confirm that a link exists in the metadata.
    res <- getFileMetadata(paste0("test-zircon-upload:blah.rds@", version), url=example.url)
    expect_identical(res$path, "blah.rds")
    linked <- res[["_extra"]][["link"]][["id"]]
    expect_match(linked, "base$")

    # Confirm that the endpoints retrieve the file successfully.
    contents <- getFile(paste0("test-zircon-upload:blah.rds@", version), url=example.url)
    expect_identical(readRDS(contents), LETTERS)
})

test_that("md5-linked uploads fail correctly (mismatch)", {
    # Making sure we force a new upload if the md5sums don't match.
    Sys.sleep(1)
    version <- as.integer(Sys.time()) 

    f <- list.files(tmp, recursive=TRUE)
    linkable <- which(!grepl(".json$", f))
    mlinks <- list()
    for (x in f[linkable]) {
        mlinks[[x]] <- "FOO"
    }
    expect_true(length(mlinks) > 1)
    remaining <- f[-linkable]

    start.url <- createUploadStartUrl(example.url, "test-zircon-upload", version)
    info <- initializeUpload(tmp, remaining, start.url, dedup.md5=mlinks, dedup.md5.field="md5sum", expires=1)

    parsed <- httr::content(info)
    expect_true(all(f %in% names(parsed$presigned_urls)))
    expect_true(length(parsed$links) == 0L)

    # abortUpload(example_url, parsed)
})

test_that("md5-linked uploads fail correctly (missing files)", {
    Sys.sleep(1)
    version <- as.integer(Sys.time()) 

    tmp2 <- tempfile()
    dir.create(tmp2)

    # Making sure we force a new upload if the file doesn't exist.
    file.copy(file.path(tmp, "whee.rds"), file.path(tmp2, "aaa.rds"))
    file.copy(file.path(tmp, "whee.rds.json"), file.path(tmp2, "aaa.rds.json"))

    f <- list.files(tmp2, recursive=TRUE)
    linkable <- which(!grepl(".json$", f))
    mlinks <- create_md5_links(tmp2, f[linkable])
    expect_true(length(mlinks) > 0)

    start.url <- createUploadStartUrl(example.url, "test-zircon-upload", version)
    info <- initializeUpload(tmp2, f[-linkable], start.url, dedup.md5=mlinks, dedup.md5.field="md5sum", expires=1)

    parsed <- httr::content(info)
    expect_true(all(f %in% names(parsed$presigned_urls)))
    expect_true(length(parsed$links) == 0L)

    # abortUpload(example_url, parsed)
})

test_that("md5-linked uploads fail correctly (missing project)", {
    Sys.sleep(1)
    version <- as.integer(Sys.time()) 

    f <- list.files(tmp, recursive=TRUE)
    linkable <- which(!grepl(".json$", f))
    mlinks <- create_md5_links(tmp, f[linkable])
    expect_true(length(mlinks) > 1)
    remaining <- f[-linkable]

    # Creating a whole other version.
    start.url <- createUploadStartUrl(example.url, "test-zircon-upload2", version)
    info <- initializeUpload(tmp, remaining, start.url, dedup.md5=mlinks, dedup.md5.field="md5sum", expires=1)

    parsed <- httr::content(info)
    expect_true(all(f %in% names(parsed$presigned_urls)))
    expect_true(length(parsed$links) == 0L)
})

test_that("manually linked uploads work correctly", {
    Sys.sleep(1)
    version <- as.integer(Sys.time()) 

    f <- list.files(tmp, recursive=TRUE)
    linkable <- which(!grepl(".json$", f))
    alinks <- list()
    for (x in f[linkable]) {
        alinks[[x]] <- packID("test-zircon-upload", x, "base")
    }
    expect_true(length(alinks) > 1)
    remaining <- f[-linkable]

    start.url <- createUploadStartUrl(example.url, "test-zircon-upload", version)
    info <- initializeUpload(tmp, remaining, start.url, dedup.link=alinks, dedup.md5.field="md5sum", expires=1)

    parsed <- httr::content(info)
    expect_true(all(f[linkable] %in% names(parsed$links)))
    expect_true(all(remaining %in% names(parsed$presigned_urls)))

    uploadFiles(tmp, example.url, parsed)
    comp <- completeUpload(example.url, parsed)

    # Confirm that a link exists in the metadata.
    res <- getFileMetadata(paste0("test-zircon-upload:blah.rds@", version), url=example.url)
    expect_identical(res$path, "blah.rds")
    linked <- res[["_extra"]][["link"]][["id"]]
    expect_type(linked, "character")
    expect_identical(unpackID(linked)$version, "base")

    # Confirm that the endpoints retrieve the file successfully.
    contents <- getFile(paste0("test-zircon-upload:blah.rds@", version), url=example.url)
    expect_identical(readRDS(contents), LETTERS)
})

test_that("manually linked uploads fail for expirable projects", {
    Sys.sleep(1)
    version <- as.integer(Sys.time()) 

    f <- list.files(tmp, recursive=TRUE)
    linkable <- which(!grepl(".json$", f))
    alinks <- list()
    for (x in f[linkable]) {
        alinks[[x]] <- packID("test-zircon-upload", x, first_version)
    }
    expect_true(length(alinks) > 1)
    remaining <- f[-linkable]

    start.url <- createUploadStartUrl(example.url, "test-zircon-upload", version)
    expect_error(initializeUpload(tmp, remaining, start.url, dedup.link=alinks, dedup.md5.field="md5sum", expires=1), "transient")
})

identityAvailable(olda)
identityHeaders(oldh)
