# This tests the upload machinery based on the private key.
# library(testthat); library(zircon); source("setup-private.R"); source("setup.R"); source("test-upload.R")

fun <- setGithubIdentities()
if (is.null(fun)) {
    skip("missing a GITHUB_TOKEN environment variable for uploads")
}

src <- system.file("scripts", "mock.R", package="zircon")
source(src)
tmp <- tempfile()
createMockProject(tmp)

first_version <- as.integer(Sys.time())
test_that("basic upload sequence works correctly", {
    f <- list.files(tmp, recursive=TRUE)

    start.url <- createUploadStartURL(example.url, "test-zircon-upload", first_version)
    info <- initializeUpload(tmp, f, start.url, expires=1)

    parsed <- httr::content(info)
    expect_true(all(f %in% names(parsed$presigned_urls)))

    uploadFiles(tmp, example.url, parsed)
    comp <- completeUpload(example.url, parsed)

    res <- getFileMetadata(paste0("test-zircon-upload:blah.txt@", first_version), url=example.url)
    expect_identical(res$path, "blah.txt")

    contents <- getFile(paste0("test-zircon-upload:blah.txt@", first_version), url=example.url)
    expect_identical(readLines(contents), LETTERS)
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

    start.url <- createUploadStartURL(example.url, "test-zircon-upload", version)
    info <- initializeUpload(tmp, remaining, start.url, dedup.md5=mlinks, dedup.md5.field="md5sum", expires=1)

    parsed <- httr::content(info)
    expect_true(all(f[linkable] %in% names(parsed$links)))
    expect_true(all(remaining %in% names(parsed$presigned_urls)))

    uploadFiles(tmp, example.url, parsed)
    comp <- completeUpload(example.url, parsed)

    # Confirm that a link exists in the metadata.
    res <- getFileMetadata(paste0("test-zircon-upload:blah.txt@", version), url=example.url)
    expect_identical(res$path, "blah.txt")
    linked <- res[["_extra"]][["link"]][["artifactdb"]]
    expect_match(linked, "base$")

    # Confirm that the endpoints retrieve the file successfully.
    contents <- getFile(paste0("test-zircon-upload:blah.txt@", version), url=example.url)
    expect_identical(readLines(contents), LETTERS)
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

    start.url <- createUploadStartURL(example.url, "test-zircon-upload", version)
    info <- initializeUpload(tmp, remaining, start.url, dedup.md5=mlinks, dedup.md5.field="md5sum", expires=1)

    parsed <- httr::content(info)
    expect_true(all(f %in% names(parsed$presigned_urls)))
    expect_true(length(parsed$links) == 0L)

    abortUpload(example.url, parsed)
})

test_that("md5-linked uploads fail correctly (missing files)", {
    Sys.sleep(1)
    version <- as.integer(Sys.time()) 

    tmp2 <- tempfile()
    dir.create(tmp2)

    # Making sure we force a new upload if the file doesn't exist.
    file.copy(file.path(tmp, "whee.txt"), file.path(tmp2, "aaa.txt"))
    file.copy(file.path(tmp, "whee.txt.json"), file.path(tmp2, "aaa.txt.json"))

    f <- list.files(tmp2, recursive=TRUE)
    linkable <- which(!grepl(".json$", f))
    mlinks <- create_md5_links(tmp2, f[linkable])
    expect_true(length(mlinks) > 0)

    start.url <- createUploadStartURL(example.url, "test-zircon-upload", version)
    info <- initializeUpload(tmp2, f[-linkable], start.url, dedup.md5=mlinks, dedup.md5.field="md5sum", expires=1)

    parsed <- httr::content(info)
    expect_true(all(f %in% names(parsed$presigned_urls)))
    expect_true(length(parsed$links) == 0L)

    abortUpload(example.url, parsed)
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
    start.url <- createUploadStartURL(example.url, "test-zircon-upload2", version)
    info <- initializeUpload(tmp, remaining, start.url, dedup.md5=mlinks, dedup.md5.field="md5sum", expires=1)

    parsed <- httr::content(info)
    expect_true(all(f %in% names(parsed$presigned_urls)))
    expect_true(length(parsed$links) == 0L)

    abortUpload(example.url, parsed)
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

    start.url <- createUploadStartURL(example.url, "test-zircon-upload", version)
    info <- initializeUpload(tmp, remaining, start.url, dedup.link=alinks, dedup.md5.field="md5sum", expires=1)

    parsed <- httr::content(info)
    expect_true(all(f[linkable] %in% names(parsed$links)))
    expect_true(all(remaining %in% names(parsed$presigned_urls)))

    uploadFiles(tmp, example.url, parsed)
    comp <- completeUpload(example.url, parsed)

    # Confirm that a link exists in the metadata.
    res <- getFileMetadata(paste0("test-zircon-upload:blah.txt@", version), url=example.url)
    expect_identical(res$path, "blah.txt")
    linked <- res[["_extra"]][["link"]][["artifactdb"]]
    expect_type(linked, "character")
    expect_identical(unpackID(linked)$version, "base")

    # Confirm that the endpoints retrieve the file successfully.
    contents <- getFile(paste0("test-zircon-upload:blah.txt@", version), url=example.url)
    expect_identical(readLines(contents), LETTERS)
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

    start.url <- createUploadStartURL(example.url, "test-zircon-upload", version)
    expect_error(initializeUpload(tmp, remaining, start.url, dedup.link=alinks, dedup.md5.field="md5sum", expires=1), "transient")
})

fun() # resetting identities at the end.
