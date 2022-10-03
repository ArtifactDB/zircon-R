# R client for ArtifactDB 

The **zircon** package implements an R client for interacting with an ArtifactDB REST API.
It provides methods for downloading files and metadata, inspecting project versions, and uploading project artifacts.
Developers can customize caching and authentication according to their specific ArtifactDB instance.
To get started, install the package and its dependencies from GitHub:

```r
devtools::install_github("ArtifactDB/zircon-R")
```

We'll use examples from the test ArtifactDB API to demonstrate:

```r
library(zircon)

example.url
## [1] "https://gypsum-test.aaron-lun.workers.dev"
example.id
## [1] "test-zircon-upload:blah.txt@base"

str(getFileMetadata(example.id, example.url))
## List of 5
##  $ $schema     : chr "generic_file/v1.json"
##  $ generic_file:List of 1
##   ..$ format: chr "text"
##  $ md5sum      : chr "0eb827652a5c272e1c82002f1c972018"
##  $ path        : chr "blah.txt"
##  $ _extra      :List of 9
##   ..$ $schema      : chr "generic_file/v1.json"
##   ..$ id           : chr "test-zircon-upload:blah.txt@base"
##   ..$ project_id   : chr "test-zircon-upload"
##   ..$ version      : chr "base"
##   ..$ metapath     : chr "blah.txt"
##   ..$ meta_indexed : chr "2022-10-01T19:45:51.913Z"
##   ..$ meta_uploaded: chr "2022-10-01T19:45:34.442Z"
##   ..$ uploaded     : chr "2022-10-01T19:45:34.442Z"
##   ..$ permissions  :List of 5
##   .. ..$ scope       : chr "project"
##   .. ..$ read_access : chr "public"
##   .. ..$ write_access: chr "owners"
##   .. ..$ owners      : chr "ArtifactDB-bot"
##   .. ..$ viewers     : list()

readLines(getFile(example.id, example.url))
##  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
## [20] "T" "U" "V" "W" "X" "Y" "Z"

listProjectVersions(example.project, example.url)
## $versions
## [1] "base"
## 
## $latest
## [1] "base"

# Creating a mock project for upload:
src <- system.file("scripts", "mock.R", package="zircon")
source(src)
tmp <- tempfile()
createMockProject(tmp)
uploadProject(tmp, example.url, "test-zircon-upload", "foobar")
```

See the [user's guide](https://artifactdb.github.io/zircon-R/articles/userguide.html) for more details. 
