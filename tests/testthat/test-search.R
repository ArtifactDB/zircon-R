# This tests the search capabilities of ArtifactDB.
# library(testthat); library(zircon); source("setup.R"); source("test-search.R")

test_that("searchMetadata works as expected", {
    searched <- searchMetadata(example.project, url=example.url, warn.auth=FALSE)
    expect_true(length(searched) > 1)
    projects <- vapply(searched, function(x) x$`_extra`$project_id, "")
    expect_true(any(example.project == projects))

    # Constricted to the desired fields.
    searched <- searchMetadata(example.project, fields="path", url=example.url, warn.auth=FALSE)
    expect_true(length(searched) > 1)

    returned <- lapply(searched, names)
    returned <- unique(unlist(returned))
    returned <- returned[!grepl("_extra", returned)]
    expect_identical(returned, "path")

    # Only gets the requested number.
    searched <- searchMetadata(example.project, url=example.url, number=1, warn.auth=FALSE)
    expect_true(length(searched)==1)
})

test_that("searchMetadata scrolls works correctly", {
    skip("not enough assets")
    expect_warning(out <- searchMetadata("luna", field=c("path", "description"), url=.getExampleURL()), "truncated")
    expect_warning(more <- searchMetadata("luna", field=c("path", "description"), url=.getExampleURL(), n=100), NA)
    expect_true(length(more) > length(out))
    expect_true(length(more) > 50)
})

test_that("searchMetadata warns... but does not fail for public files", {
    skip("no authentication yet available")
    expect_warning(searched <- searchMetadata(thing, url=example.url), "non-authenticated")
    expect_true(length(searched) > 0)

    # By comparison, it flies through if a token is provided.
    # SET TOKEN HERE
    expect_warning(searched <- .searchMetadata(thing, url=example.url, n=100), NA)
    expect_true(length(searched) > 1)
})

test_that("dash insertion works as expected", {
    str <- "Asdasdasd AND bsadasd"
    expect_identical(zircon:::.inject_dashes(str), str) # no-op

    expect_identical(zircon:::.inject_dashes("test-public"), "\"test-public\"")
    expect_identical(zircon:::.inject_dashes("\"test-public\" OR asdasd-asdasd"), "\"test-public\" OR \"asdasd-asdasd\"")
    expect_identical(zircon:::.inject_dashes("(test-public)"), "(\"test-public\")")
    expect_identical(zircon:::.inject_dashes("(a-b-c)"), "(\"a-b-c\")")
    expect_identical(zircon:::.inject_dashes("(test-public AND foobar)"), "(\"test-public\" AND foobar)")
    expect_identical(zircon:::.inject_dashes("(whee:test-public)"), "(whee:\"test-public\")")
    expect_identical(zircon:::.inject_dashes("whee:test-public OR bar:aaron-test"), "whee:\"test-public\" OR bar:\"aaron-test\"")
    expect_identical(zircon:::.inject_dashes("whee:\"test-public-test\""), "whee:\"test-public-test\"")
})

