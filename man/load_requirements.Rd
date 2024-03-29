% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_requirements.R
\name{load_requirements}
\alias{load_requirements}
\title{Loading/Attaching all packages by version specified in a requirements-file}
\usage{
load_requirements(
  req.file.path = getwd(),
  req.file.name = "requirements.txt",
  lists = "all",
  library.folder.path = getwd(),
  library.folder.name = "lib"
)
}
\arguments{
\item{req.file.path}{(chr value): Name of the requirements-file}

\item{req.file.name}{(chr value): Folder of the requirements-file}

\item{lists}{(chr vector): Names of lists (without '#' !) specified in the requirements-file to use}

\item{library.folder.path}{(chr value): Path in which the folder is located from which the packages are to be loaded}

\item{library.folder.name}{(chr value): Name of the lib-folder}
}
\description{
Loading/Attaching all packages by version specified in a requirements-file
}
\details{
Wrapper around get_requirements() and library_version(). Loading/Attatching all packages from the requirements-file.
}
\section{Dependencies}{

- update_packages_search_path()

- get_requirements()

- library_version()
}

\section{Side effects}{
 Loading/Attatching all packages from the requirements-file.
}

\section{Return}{
 None
}

\examples{
\dontrun{
# load all packages specified in the 'requirements.txt.' within the current working directory
detach_none_base() # detach all none base packages
load_requirements()
as.vector(apply(sapply(sessionInfo()$otherPkgs, \(x) x[c("Package", "Version")]), 2, \(x) paste(x, collapse = "_"))) # currently loaded packages incl. version


# load packages specified as main packages AND packages specified within the list '#statistics' in the 'requirements.txt.' within the current working directory
detach_none_base() # detach all none base packages
load_requirements(lists = "#statistics")
as.vector(apply(sapply(sessionInfo()$otherPkgs, \(x) x[c("Package", "Version")]), 2, \(x) paste(x, collapse = "_"))) # currently loaded packages incl. version


# load packages specified as main packages AND packages specified within the lists '#statistics' & '#data_wrangling' in the 'requirements.txt.' within the current working directory
detach_none_base() # detach all none base packages
load_requirements(lists = c("#statistics", "#data_wrangling"))
as.vector(apply(sapply(sessionInfo()$otherPkgs, \(x) x[c("Package", "Version")]), 2, \(x) paste(x, collapse = "_"))) # currently loaded packages incl. version
}

}
\seealso{
\code{\link[base]{.libPaths()}}
}
\author{
Simon Ress
}
\keyword{library}
\keyword{loading}
\keyword{package-versions}
\keyword{requirements-file}
