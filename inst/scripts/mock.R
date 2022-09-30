createMockProject <- function(dir) {
    basic <- list(
        `$schema`="generic_file/v1.json",
        generic_file = list(
            format="text"
        )
    )

    dir.create(dir)

    rpath1 <- "whee.txt"
    rpath10 <- paste0(rpath1, ".json")
    fpath1 <- file.path(dir, rpath1)
    writeLines(con=fpath1, letters)
    fpath10 <- file.path(dir, rpath10)
    md5.1 <- digest::digest(file=fpath1)
    write(file=fpath10, jsonlite::toJSON(c(basic, list(md5sum=md5.1, path=basename(fpath1))), auto_unbox=TRUE, pretty=TRUE))

    rpath2 <- "blah.txt"
    rpath20 <- paste0(rpath2, ".json")
    fpath2 <- file.path(dir, rpath2)
    writeLines(con=fpath2, LETTERS)
    fpath20 <- file.path(dir, rpath20)
    md5.2 <- digest::digest(file=fpath2)
    write(file=fpath20, jsonlite::toJSON(c(basic, list(md5sum=md5.2, path=basename(fpath2))), auto_unbox=TRUE, pretty=TRUE))

    rpath3 <- "foo/bar.txt"
    rpath30 <- paste0(rpath3, ".json")
    dir.create(file.path(dir, "foo"))
    fpath3 <- file.path(dir, rpath3)
    writeLines(con=fpath3, as.character(1:100))
    md5.3 <- digest::digest(file=fpath3)
    fpath30 <- file.path(dir, rpath30)
    write(file=fpath30, jsonlite::toJSON(c(basic, list(md5sum=md5.3, path=basename(fpath3))), auto_unbox=TRUE, pretty=TRUE))
}
