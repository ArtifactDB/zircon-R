# Generate the contents for upload to a test bucket.
# library(zircon); source("setup-private.R"); source("setup.R"); 
# source("fresh-upload.R")

src <- system.file("scripts", "mock.R", package="zircon")
source(src)
tmp <- tempfile()
createMockProject(tmp)

setGithubIdentities()

# Base persistent upload, for reliable testing.
uploadProject(tmp, example.url, "test-public", "base")

# Test project for private permissions.
uploadProject(tmp, example.url, "test-private", "base", permissions=list(read_access="viewers"))

# Upload a new version of the public dataset.
tmp2 <- tempfile()
createMockProject(tmp2)
write("Aaron Lun had a little lamb.", file=file.path(tmp2, "whee.txt"))
meta <- jsonlite::fromJSON(file.path(tmp2, "whee.txt.json"))
meta$md5sum <- digest::digest(file=file.path(tmp2, "whee.txt"))
write(jsonlite::toJSON(meta, auto_unbox=TRUE), file=file.path(tmp2, "whee.txt.json"))
uploadProject(tmp2, example.url, "test-public", "modified")

# Test project for links and redirects.
tmp2 <- tempfile()
createMockProject(tmp2)
createRedirection(tmp2, "redirect", "foo/bar.txt")

f <- list.files(tmp2, recursive=TRUE)
keep <- grepl(".json$", f)
remaining <- f[keep]
to_link <- f[!keep]
nms <- packID("test-public", to_link, "base")
uploadProject(tmp2, example.url, "test-links", "public", files=remaining, dedup.link=setNames(nms, to_link))

f <- list.files(tmp2, recursive=TRUE)
keep <- grepl(".json$", f)
remaining <- f[keep]
to_link <- f[!keep]
nms <- packID("test-private", to_link, "base")
uploadProject(tmp2, example.url, "test-links", "private", files=remaining, dedup.link=setNames(nms, to_link))
