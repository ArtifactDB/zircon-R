# Generate the contents for upload to a test bucket.
# source("setup-private.R"); source("setup.R"); source("fresh-upload.R")

src <- system.file("scripts", "mock.R", package="zircon")
source(src)
tmp <- tempfile()
createMockProject(tmp)

setGithubIdentities()

# Base persistent upload, for reliable testing.
uploadProject(tmp, example.url, "test-zircon-upload", "base")

# Test project for permissions.
uploadProject(tmp, example.url, "test-zircon-permissions", "base")

# Test project for links.
f <- list.files(tmp, recursive=TRUE)
keep <- grepl(".json$", f)
remaining <- f[keep]
to_link <- f[!keep]
nms <- packID("test-zircon-upload", to_link, "base")
uploadProject(tmp, example.url, "test-zircon-link", "base", files=remaining, dedup.link=setNames(nms, to_link))
