# Test that the file metadata getters work as expected.
# library(testthat); library(zircon); source("setup.R"); source("setup-private.R"); source("test-permissions.R")

test_that("permissions getters work correctly", {
    out <- getPermissions(example.project, example.url)

    expect_identical(out$read_access, "public")
    expect_identical(out$write_access, "owners")

    expect_type(out$owners, "character")
    expect_true(length(out$owners) > 0L)
    expect_type(out$viewers, "character")

    expect_identical(out$scope, "project")
})

test_that("permissions getters fail correctly", {
    # This project is private and should not have access granted to anonymous users.
    expect_error(getPermissions("test-private", example.url), "credentials not supplied")
})

test_that("permissions setters work correctly", {
    fun <- setGithubIdentities()
    if (is.null(fun)) {
        skip("no credentials for testing permission setting")
    }
    on.exit(fun())

    example.project2 <- "test-zircon-permissions"

    setPermissions(example.project2, example.url, public=TRUE, viewers="lawremi", action="append")
    Sys.sleep(3) # Wait for async propagation.
    out <- getPermissions(example.project2, example.url)
    expect_identical(out$read_access, "public")
    expect_true("lawremi" %in% out$viewers)

    setPermissions(example.project2, example.url, public=FALSE, viewers="lawremi", action="remove")
    Sys.sleep(3) # Again, wait for async propagation.
    out <- getPermissions(example.project2, example.url)
    expect_identical(out$read_access, "viewers")
    expect_false("lawremi" %in% out$viewers)
})

