# This tests the symlink detection mechanism is working.
# library(testthat); library(zircon); source("test-symlinks.R")

test_that("symlink creation and extraction works as expected", {
    tmp <- tempfile()
    dir.create(tmp)

    createPlaceholderLink(tmp, "boo.txt", example.id)
    expect_identical(extractLinkedID(tmp, "boo.txt"), example.id)

    # Fails if the target is present, as this might be intended to be a real symlink.
    write(file=Sys.readlink(file.path(tmp, "boo.txt")), character(0))
    expect_error(extractLinkedID(tmp, "boo.txt"), "dangling")
})

test_that("symlinks are correctly detected on upload initialization", {
    tmp <- tempfile()
    dir.create(tmp)

    write(file=file.path(tmp, "foo.json"), LETTERS)
    file.symlink(system.file("scripts", "mock.R", package="zircon"), file.path(tmp, "mock.json"))
    createPlaceholderLink(tmp, "boo.txt", example.id)
    formatted <- zircon:::.format_files(tmp, c("foo.json", "mock.json", "boo.txt"), auto.dedup.md5=FALSE, md5.field="md5sum")

    # Simple things work...
    expect_identical(formatted[[1]]$check, "simple")
    expect_identical(formatted[[1]]$filename, "foo.json")
    expect_identical(formatted[[2]]$check, "simple")
    expect_identical(formatted[[2]]$filename, "mock.json")

    # And so do the links.
    expect_identical(formatted[[3]]$check, "link")
    expect_identical(formatted[[3]]$filename, "boo.txt")
    expect_identical(formatted[[3]]$value$artifactdb_id, example.id)
})

test_that("symlinks to metadata fail on upload initialization", {
    tmp <- tempfile()
    dir.create(tmp)
    createPlaceholderLink(tmp, "boo.json", example.id)
    expect_error(zircon:::.format_files(tmp, c("boo.json")), "cannot create link")
})
