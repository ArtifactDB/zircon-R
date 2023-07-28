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
        github.cache.path=if (cache) cache.path else NULL
    )
}

accessTokenInfo <- function() {
    getGitHubTokenInfo(prompt=FALSE, 
        cache.env=cache.env, 
        github.cache.path=cache.path
    )
}

test_that("Github token setting works correctly without caching", {
    unlink(cache.path)

    info <- setAccessToken(token, cache=FALSE)
    expect_type(info$name, "character")
    expect_type(info$token, "character")
    expect_type(info$expires, "double")

    expect_identical(info, accessTokenInfo())
    expect_false(file.exists(cache.path))
})

test_that("Github token setting works correctly with caching", {
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

test_that("GitHub token wiping works correctly with caching", {
    write(file=cache.path, character(0))
    info <- setAccessToken(NULL)
    expect_null(accessTokenInfo())
    expect_false(file.exists(cache.path)) # wipes the cached file.
    unlink(cache.path)
})

jwt.cache.path <- tempfile(fileext=".txt")

setAccessToken(token)
jwtAccessTokenInfo <- function(cache = FALSE) {
    getJWTFromGitHub(prompt=FALSE, 
        cache.env=cache.env, 
        org.id="CollaboratorDB",
        jwt.cache.path=if (cache) jwt.cache.path else NULL,
        github.cache.path=cache.path
    )
}

test_that("JWT token setting works correctly without caching", {
    unlink(jwt.cache.path)
    cache.env$jwt <- NULL

    info <- jwtAccessTokenInfo()
    expect_type(info$token, "character")
    expect_type(info$expires, "double")
    expect_false(file.exists(jwt.cache.path))
})

test_that("JWT token setting works correctly with caching", {
    unlink(jwt.cache.path)
    cache.env$jwt <- NULL

    info <- jwtAccessTokenInfo(TRUE)
    expect_type(info$token, "character")
    expect_type(info$expires, "double")
    expect_true(file.exists(jwt.cache.path))

    # Fetches it from cache.
    expect_identical(cache.env$jwt$token, info$token) # checking that we're wiping the right thing.
    cache.env$jwt <- NULL
    expect_identical(info, jwtAccessTokenInfo(TRUE))
})


