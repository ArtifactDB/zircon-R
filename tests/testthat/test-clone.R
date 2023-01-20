# This tests that the project cloning works as expected
# library(testthat); library(zircon); source("setup.R"); source("test-clone.R")

test_that("project cloning works as expected (simple)", {
    tmp <- tempfile()
    cloneProjectVersion(tmp, example.url, example.project, example.version, link.only=FALSE)

    expect_identical(readLines(file.path(tmp, "blah.txt")), LETTERS)
    expect_identical(jsonlite::fromJSON(file.path(tmp, "blah.txt.json"))$path, "blah.txt")

    expect_identical(readLines(file.path(tmp, "foo/bar.txt")), as.character(1:100))
    expect_identical(jsonlite::fromJSON(file.path(tmp, "foo/bar.txt.json"))$path, "foo/bar.txt")
})

test_that("project cloning works as expected (links)", {
    tmp <- tempfile()
    cloneProjectVersion(tmp, example.url, example.project, example.version, link.only=TRUE)

    expect_identical(jsonlite::fromJSON(file.path(tmp, "whee.txt.json"))$path, "whee.txt")
    expect_identical(extractLinkedID(tmp, "whee.txt"), packID(example.project, "whee.txt", example.version))
    expect_identical(extractLinkedID(tmp, "foo/bar.txt"), packID(example.project, "foo/bar.txt", example.version))
})

test_that("project cloning works as expected (cache)", {
    tmp <- tempfile()
    cachedCounter$hits <- 0L
    cloneProjectVersion(tmp, example.url, example.project, example.version, cache=cacheTemporary)
    expect_identical(readLines(file.path(tmp, "whee.txt")), letters)

    # Next call should hit the cache.
    tmp2 <- tempfile()
    cloneProjectVersion(tmp2, example.url, example.project, example.version, cache=cacheTemporary)
    expect_identical(readLines(file.path(tmp2, "whee.txt")), letters)
    expect_true(cachedCounter$hits > 0L)
})

test_that("project cloning behaves with redirections", {
    tmp <- tempfile()
    cloneProjectVersion(tmp, example.url, "test-links", "public", cache=cacheTemporary)
    expect_identical(jsonlite::fromJSON(file.path(tmp, "redirect.json"))$path, "redirect")
    expect_identical(readLines(file.path(tmp, "foo/bar.txt")), as.character(1:100))
})
