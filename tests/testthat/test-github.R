# This tests that the token getter/setter works correctly.
# library(testthat); library(zircon); source("setup-private.R"); source("test-github.R")

token <- Sys.getenv("GITHUB_TOKEN", NA)
if (is.na(token)) {
    skip("skipping authentication tests because GITHUB_TOKEN is absent")
}

cache.env <- new.env()
cache.path <- tempfile(fileext=".txt")

setAccessToken <- function(..., cache=TRUE) {
    setGitHubToken(...,
        cache.env=cache.env,
        cache.path=if (cache) cache.path else NULL
    )
}

accessTokenInfo <- function() {
    getGitHubTokenInfo(prompt=FALSE, 
        cache.env=cache.env, 
        cache.path=cache.path
    )
}

test_that("token setting works correctly without caching", {
    unlink(cache.path)

    info <- setAccessToken(token, cache=FALSE)
    expect_type(info$name, "character")
    expect_type(info$token, "character")
    expect_type(info$expires, "double")

    expect_identical(info, accessTokenInfo())
    expect_false(file.exists(cache.path))
})

test_that("token setting works correctly with caching", {
    info <- setAccessToken(token)
    expect_type(info$name, "character")
    expect_type(info$token, "character")
    expect_type(info$expires, "double")

    expect_identical(info, accessTokenInfo())
    expect_true(file.exists(cache.path))

    # Wiping it in local memory.
    setAccessToken(NULL, cache=FALSE)
    expect_null(cache.env$auth.info)

    # Fetches it from cache.
    expect_identical(info, accessTokenInfo())
})

test_that("token wiping works correctly with caching", {
    write(file=cache.path, character(0))
    info <- setAccessToken(NULL)
    expect_null(accessTokenInfo())
    expect_false(file.exists(cache.path)) # wipes the cached file.
    unlink(cache.path)
})
