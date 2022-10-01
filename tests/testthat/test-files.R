# Test that the file metadata getters work as expected.
# library(testthat); library(zircon); source("setup.R"); source("test-files.R")

test_that("file metadata getters work correctly", {
    X <- getFileMetadata(example.id, url = example.url)

    unpacked <- unpackID(example.id)
    expect_identical(X$path, unpacked$path)
    expect_identical(X[["_extra"]][["version"]], unpacked$version)
    expect_identical(X[["_extra"]][["project_id"]], unpacked$project)

    # Report links.
    linked <- getFileMetadata(packID("test-zircon-link", "foo/bar.txt", "base"), example.url)
    unpack.link <- unpackID(linked[["_extra"]][["link"]][["artifactdb"]])
    expect_identical(unpack.link$project, example.project)
})

test_that("file metadata getters work correctly with caching", {
    cachedCounter$hits <- 0L
    X1 <- getFileMetadata(example.id, example.url, cache = cacheTemporary)
    X2 <- getFileMetadata(example.id, example.url, cache = cacheTemporary) 
    expect_identical(X1, X2)
    expect_true(cachedCounter$hits > 0)
})

test_that("file metadata getters work correctly with the latest alias", {
    unpack <- unpackID(example.id)
    unpack$version <- "latest"
    repack <- do.call(packID, unpack)

    latest <- getFileMetadata(repack, example.url)
    unpack2 <- unpackID(latest[["_extra"]][["id"]])
    expect_false(unpack2$version == "latest")

    ref <- getFileMetadata(latest[["_extra"]][["id"]], example.url)
    expect_identical(ref, latest)

    # Doesn't cache it under a 'latest' key.
    cache <- getFileMetadata(repack, example.url, cache = cacheTemporary)
    expect_identical(ref, cache)
    expect_false(any(grepl("latest", list.files(tmp.cache))))
})

test_that("file getters work correctly", {
    X <- getFile(example.id, url = example.url)
    expect_identical(readLines(X), LETTERS)

    # Respects user-supplied path.
    tmp <- tempfile(fileext="blah.txt")
    X2 <- getFile(example.id, path=tmp, url = example.url)
    expect_identical(X2, tmp)
    expect_identical(readLines(tmp), LETTERS)
})

test_that("file url getters work correctly", {
    X <- getFileURL(example.id, url = example.url)
    expect_type(X, "character")
})

test_that("file getters work correctly with caching", {
    cachedCounter$hits <- 0L
    X1 <- getFile(example.id, example.url, cache = cacheTemporary)
    X2 <- getFile(example.id, example.url, cache = cacheTemporary) 
    expect_identical(X1, X2)
    expect_true(cachedCounter$hits > 0)
})

test_that("file getters work correctly with the latest alias", {
    unpack <- unpackID(example.id)
    unpack$version <- "latest"
    repack <- do.call(packID, unpack)

    latest <- getFile(repack, example.url)
    expect_identical(readLines(X), LETTERS)

    # Doesn't cache it under a 'latest' key.
    cached <- getFile(repack, example.url, cache = cacheTemporary)
    expect_false(grepl("latest", cached))
})

test_that("file getters follow links correctly", {
    link.path <- getFile(packID("test-zircon-link", "foo/bar.txt", "base"), example.url)
    expect_identical(readLines(link.path), as.character(1:100))

    # With caching, both the original and duplicate files are generated in the cache upon following the redirects.
    cachedCounter$hits <- 0L
    unlink(list.files(tmp.cache, recursive=TRUE, full=TRUE))
    link.path <- getFile(packID("test-zircon-link", "foo/bar.txt", "base"), example.url, cache=cacheTemporary)
    expect_identical(readLines(link.path), as.character(1:100))

    f <- list.files(tmp.cache)
    expect_true(any(grepl("test-zircon-link", f)))
    expect_true(any(grepl("test-zircon-upload", f)))

    expect_identical(cachedCounter$hits, 0L)
    base <- getFile(packID("test-zircon-upload", "foo/bar.txt", "base"), example.url, cache=cacheTemporary)
    expect_true(cachedCounter$hits > 0L)
})
