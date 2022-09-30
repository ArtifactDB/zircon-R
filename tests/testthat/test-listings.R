# Test that the listing endpoints work as expected.
# library(testthat); library(zircon); source("setup.R"); source("test-listings.R")

test_that("listProjectVersions work as expected", {
    x <- listProjectVersions(example.project, example.url)
    expect_true(example.version %in% x$versions)
    expect_type(x$latest, "character")
})

test_that("listProjects work as expected", {
    x <- listProjects(example.url, number=10)
    expect_true(length(x) <= 10)

    if (example.project %in% names(x)) {
        expect_true(example.version %in% x[[example.project]])
    }
})

