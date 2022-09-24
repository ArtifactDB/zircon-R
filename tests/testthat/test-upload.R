# This tests the upload machinery based on the private key.
# library(testthat); library(zircon); source("setup-private.R"); source("test-upload.R")

token <- Sys.getenv("GITHUB_TOKEN", NA)
if (is.na(token)) {
    skip("missing a GITHUB_TOKEN environment variable for uploads")
}
olda <- identityAvailable(function() TRUE)
oldh <- identityHeaders(function() list(Authorization=paste0("Bearer ", token)))
on.exit({
    identityAvailable(olda)
    identityHeaders(oldh)
})

basic <- list(
    origin=list(
        list(type="DataSetDB", id="DS000000267", experiment="RNA-Seq_hsa_gene", version=1)
    ),
    `$schema`="generic_object/v1.json",
    annotated_object = list(
        language="R",
        class="letters"
    )
)

tmp <- "foo-test" # tempfile()
dir.create(tmp)

rpath1 <- "whee.rds"
rpath10 <- paste0(rpath1, ".json")
fpath1 <- file.path(tmp, rpath1)
saveRDS(file=fpath1, letters)
fpath10 <- file.path(tmp, rpath10)
md5.1 <- digest::digest(file=fpath1)
write(file=fpath10, jsonlite::toJSON(c(basic, list(md5sum=md5.1, path=basename(fpath1), description="WHEE")), auto_unbox=TRUE, pretty=TRUE))

rpath2 <- "blah.rds"
rpath20 <- paste0(rpath2, ".json")
fpath2 <- file.path(tmp, rpath2)
saveRDS(file=fpath2, LETTERS)
fpath20 <- file.path(tmp, rpath20)
md5.2 <- digest::digest(file=fpath2)
write(file=fpath20, jsonlite::toJSON(c(basic, list(md5sum=md5.2, path=basename(fpath2), description="BLAH")), auto_unbox=TRUE, pretty=TRUE))

rpath3 <- "foo/bar.rds"
rpath30 <- paste0(rpath3, ".json")
dir.create(file.path(tmp, "foo"))
fpath3 <- file.path(tmp, rpath3)
saveRDS(file=fpath3, 1:100)
md5.3 <- digest::digest(file=fpath3)
fpath30 <- file.path(tmp, rpath30)
write(file=fpath30, jsonlite::toJSON(c(basic, list(md5sum=md5.3, path=basename(fpath3), description="FOO BAR")), auto_unbox=TRUE, pretty=TRUE))

test_that("basic upload sequence works correctly", {
        library(testthat); library(zircon); 
        tmp <- "foo-test"
        example.url <- "http://127.0.0.1:8787"

    start.url <- createUploadStartUrl(example.url, "test-zircon-upload", as.integer(Sys.time()))
    info <- initializeUpload(tmp, list.files(tmp, recursive=TRUE), start.url)
    parsed <- httr::content(info)

    uploadFiles(tmp, parsed)
    completeUpload(example.url, parsed)
})

