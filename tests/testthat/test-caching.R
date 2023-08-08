# This tests the caching functions.
# library(testthat); library(zircon); source("test-caching.R")

test_that("simple caching function works correctly", {
    tmp <- tempfile()
    dir.create(tmp, recursive=TRUE)
    cache.fun <- function(key, save) simpleCache(tmp, key, save)

    fname1 <- getFile(example.id, example.url, cache = cache.fun)
    fname2 <- getFile(example.id, example.url, cache = cache.fun)
    expect_identical(fname1, fname2)
    expect_identical(readLines(fname1), LETTERS)

    # Truly respects the cache.
    txt <- c("Aaron", "is", "a", "little", "lamb")
    writeLines(txt, con=fname1)
    fname2 <- getFile(example.id, example.url, cache = cache.fun)
    expect_identical(fname1, fname2)
    expect_identical(readLines(fname2), txt) # not changed.

    # Unless we force an update. 
    cache.fun2 <- function(key, save) simpleCache(tmp, key, save, update=TRUE)
    fname2 <- getFile(example.id, example.url, cache = cache.fun2)
    expect_identical(fname1, fname2)
    expect_identical(readLines(fname2), LETTERS) # changed back.
})

test_that("BioC caching function works correctly", {
    library(BiocFileCache)
    tmp <- tempfile()
    bfc <- BiocFileCache(tmp, ask=FALSE)
    cache.fun <- function(key, save) biocCache(bfc, key, save)

    fname1 <- getFile(example.id, example.url, cache = cache.fun)
    fname2 <- getFile(example.id, example.url, cache = cache.fun)
    expect_identical(fname1, fname2)
    expect_identical(readLines(fname1), LETTERS)

    # Truly respects the cache.
    txt <- c("Aaron", "is", "a", "little", "lamb")
    writeLines(txt, con=fname1)
    fname2 <- getFile(example.id, example.url, cache = cache.fun)
    expect_identical(fname1, fname2)
    expect_identical(readLines(fname2), txt) # not changed.

    # Unless we force an update. 
    cache.fun2 <- function(key, save) biocCache(bfc, key, save, update=TRUE)
    fname2 <- getFile(example.id, example.url, cache = cache.fun2)
    expect_identical(fname1, fname2)
    expect_identical(readLines(fname2), LETTERS) # changed back.

    # Duplicate entries are detected and erased.
    deets <- BiocFileCache::bfcinfo(bfc)
    to.use <- which(deets$rpath == fname1)
    BiocFileCache::bfcnew(bfc, deets$rname[to.use])
    expect_warning(fname3 <- getFile(example.id, example.url, cache = cache.fun), "duplicate copies")
    expect_identical(fname3, fname2)
    deets2 <- BiocFileCache::bfcinfo(bfc)
    expect_identical(deets, deets2)
})
