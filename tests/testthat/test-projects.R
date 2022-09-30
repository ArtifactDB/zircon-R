# Test that the project metadata getters work as expected.
# library(testthat); library(zircon); source("setup.R"); source("test-projects.R")

test_that("per-version project metadata getters work correctly", {
    out <- getProjectMetadata(example.project, version=example.version, url=example.url)
    expect_true(length(out) > 0)

    # Should give something sensible.
    ref <- NULL
    for (i in seq_along(out)) {
        expect_type(out[[i]]$path, "character")
        expect_type(out[[i]][["$schema"]], "character")
        expect_type(out[[i]][["_extra"]], "list")

        if (out[[i]][["_extra"]][["id"]] == example.id) {
            ref <- out[[i]]
        }
    }

    # Should be exactly the same as the /files output.
    expect_false(is.null(ref))
    expect_identical(ref, getFileMetadata(example.id, example.url))
})

test_that("project metadata getters work correctly", {
    out <- getProjectMetadata(example.project, url=example.url)
    expect_true(length(out) > 0)

    # Should give something sensible.
    ref <- list()
    for (i in seq_along(out)) {
        expect_type(out[[i]]$path, "character")
        expect_type(out[[i]][["$schema"]], "character")
        expect_type(out[[i]][["_extra"]], "list")

        if (out[[i]][["_extra"]][["version"]] == example.version) {
            ref <- c(ref, list(out[[i]]))
        }
    }

    # Should be exactly the same as the /files output.
    expected <- getProjectMetadata(example.project, version=example.version, url=example.url)
    o1 <- order(vapply(expected, function(x) x[["path"]], ""))
    o2 <- order(vapply(ref, function(x) x[["path"]], ""))
    expect_identical(expected[o1], ref[o2])
})
