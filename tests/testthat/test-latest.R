# Test that the project metadata getters work as expected.
# library(testthat); library(zircon); source("setup.R"); source("test-latest.R")

test_that("resolveLatestVersion works correctly", {
    v <- resolveLatestVersion(example.project, url=example.url)
    expect_type(v, "character") # can't be sure what it really is.
})

test_that("resolveLatestID works correctly", {
    unpacked <- unpackID(example.id)
    unpacked$version <- "latest"

    new.id <- do.call(packID, unpacked)
    resolved <- resolveLatestID(new.id, example.url)
    unpacked2 <- unpackID(resolved)

    expect_identical(unpacked2$project, unpacked$project)
    expect_identical(unpacked2$path, unpacked$path)
    expect_false(identical(unpacked2$version, unpacked$version))
})
