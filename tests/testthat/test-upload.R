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

extract_filenames <- function(r) sort(vapply(r, function(x) x$filename, ""))

first_version <- as.integer(Sys.time())
test_that("basic upload sequence works correctly", {
    f <- list.files(tmp, recursive=TRUE)

    start.url <- createUploadStartURL(example.url, "test-zircon-upload", first_version)
    info <- initializeUpload(tmp, f, start.url, expires=1)

    parsed <- httr::content(info)
    expect_identical(sort(f), extract_filenames(parsed$presigned_urls))

    uploadFiles(tmp, example.url, parsed)
    comp <- completeUpload(example.url, parsed)

    res <- getFileMetadata(paste0("test-zircon-upload:blah.txt@", first_version), url=example.url)
    expect_identical(res$path, "blah.txt")

    contents <- getFile(paste0("test-zircon-upload:blah.txt@", first_version), url=example.url)
    expect_identical(readLines(contents), LETTERS)
})

test_that("basic upload sequence detects MD5 sum mismatch", {
    Sys.sleep(1)
    version <- as.integer(Sys.time()) 

    f <- list.files(tmp, recursive=TRUE)
    start.url <- createUploadStartURL(example.url, "test-zircon-upload", version)
    info <- initializeUpload(tmp, f, start.url, expires=1)

    parsed <- httr::content(info)
    expect_identical(sort(f), extract_filenames(parsed$presigned_urls))

    # Swapping files.
    old <- parsed$presigned_urls[[1]]$filename
    parsed$presigned_urls[[1]]$filename <- parsed$presigned_urls[[2]]$filename 
    parsed$presigned_urls[[2]]$filename <- old

    expect_error(uploadFiles(tmp, example.url, parsed, attempts=1), "failed.*MD5")
})

test_that("md5-linked uploads work correctly (valid)", {
    # Creating another version now. This assumes that we can piggy-back off the base version.
    Sys.sleep(1)
    version <- as.integer(Sys.time()) 

    f <- list.files(tmp, recursive=TRUE)
    linkable <- which(!grepl(".json$", f))
    expect_true(length(linkable) > 1)

    start.url <- createUploadStartURL(example.url, "test-zircon-upload", version)
    info <- initializeUpload(tmp, f, start.url, auto.dedup.md5=TRUE, expires=1)

    parsed <- httr::content(info)
    remaining <- f[-linkable]
    expect_identical(sort(f[linkable]), extract_filenames(parsed$links))
    expect_identical(sort(remaining), extract_filenames(parsed$presigned_urls))

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

test_that("md5-linked uploads fail correctly (file types)", {
    version <- as.integer(Sys.time()) 

    # For JSON files.
    f <- list.files(tmp, recursive=TRUE)
    linkable <- which(!grepl(".json$", f))
    meta <- f[-linkable]
    mlinks <- rep("FOO", length(meta))
    names(mlinks) <- meta

    start.url <- createUploadStartURL(example.url, "test-zircon-upload", version)
    expect_error(initializeUpload(tmp, f[linkable], start.url, dedup.md5=mlinks, expires=1), "cannot request MD5-based deduplication")

    # If MD5 is not supplied.
    expect_error(initializeUpload(tmp, f[linkable], start.url, dedup.md5=mlinks, md5.field=NULL, expires=1), "md5.field=NULL")

    # Everything is skipped if the md5 field is not present.
    formatted <- zircon:::.format_files(tmp, f, auto.dedup.md5=TRUE, md5.field="md5sum")
    types <- vapply(formatted, function(x) x$check, "")
    expect_identical(sort(unique(types)), c("md5", "simple"))

    formatted <- zircon:::.format_files(tmp, f, auto.dedup.md5=TRUE, md5.field=NULL)
    types <- vapply(formatted, function(x) x$check, "")
    expect_identical(unique(types), "simple")
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
    info <- initializeUpload(tmp, remaining, start.url, dedup.md5=mlinks, expires=1)

    parsed <- httr::content(info)
    expect_identical(sort(f), extract_filenames(parsed$presigned_urls))
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
    start.url <- createUploadStartURL(example.url, "test-zircon-upload", version)
    info <- initializeUpload(tmp2, f, start.url, auto.dedup.md5=TRUE, expires=1)

    parsed <- httr::content(info)
    expect_identical(sort(f), extract_filenames(parsed$presigned_urls))
    expect_true(length(parsed$links) == 0L)

    abortUpload(example.url, parsed)
})

test_that("md5-linked uploads fail correctly (missing project)", {
    Sys.sleep(1)
    version <- as.integer(Sys.time()) 

    f <- list.files(tmp, recursive=TRUE)

    # Creating a whole other version.
    start.url <- createUploadStartURL(example.url, "test-zircon-upload2", version)
    info <- initializeUpload(tmp, f, start.url, auto.dedup.md5=TRUE, expires=1)

    parsed <- httr::content(info)
    expect_identical(sort(f), extract_filenames(parsed$presigned_urls))
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
    info <- initializeUpload(tmp, remaining, start.url, dedup.link=alinks, expires=1)

    parsed <- httr::content(info)
    expect_identical(sort(f[linkable]), extract_filenames(parsed$links))
    expect_identical(sort(remaining), extract_filenames(parsed$presigned_urls))

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
    expect_error(initializeUpload(tmp, remaining, start.url, dedup.link=alinks, expires=1), "transient")
})

test_that("manually linked uploads fail correctly for JSON files", {
    version <- as.integer(Sys.time()) 

    f <- list.files(tmp, recursive=TRUE)
    linkable <- which(!grepl(".json$", f))
    meta <- f[-linkable]
    links <- rep("FOO", length(meta))
    names(links) <- meta

    start.url <- createUploadStartURL(example.url, "test-zircon-upload", version)
    expect_error(initializeUpload(tmp, f[linkable], start.url, dedup.link=mlinks, expires=1), "cannot request link-based deduplication")
})

fun() # resetting identities at the end.
