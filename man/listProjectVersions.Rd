% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listProjectVersions.R
\name{listProjectVersions}
\alias{listProjectVersions}
\title{List project versions}
\usage{
listProjectVersions(project, url)
}
\arguments{
\item{project}{String containing the project name.}

\item{url}{String containing the URL to the ArtifactDB REST API.}
}
\value{
List containing \code{versions}, a character vector of available versions;
and \code{latest}, a string specifying the name of the latest version.
}
\description{
List the available versions of a project.
}
\examples{
listProjectVersions(example.project, example.url)

}
\seealso{
\code{\link{listProjects}}, to obtain a listing of all projects.
}
\author{
Aaron Lun
}
