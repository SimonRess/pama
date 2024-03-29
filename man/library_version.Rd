% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/library_version.R
\name{library_version}
\alias{library_version}
\title{Loading/Attaching specific versions of packages}
\usage{
library_version(package, version, lib.search.path = NULL)
}
\arguments{
\item{package}{(chr value): Name of the Package}

\item{version}{(chr value): Version of the Package}

\item{lib.search.path}{(chr vector): In folder(s) which to search for the packages. NULL: use standard paths from `.libPaths()`}
}
\description{
Loading/Attaching specific versions of packages
}
\details{
Loads a specific version of a package. The package folder must be with a folder called <package-name>_<version> which itself is within .libPaths()[1]
}
\section{Dependencies}{

- update_packages_search_path()
- unloadRecursive()
}

\section{Side effects}{
 Loading/Attaching of the package version
}

\section{Return}{
 None
}

\examples{
\dontrun{
# Load v3.4.0 of ggplot2 from folder "ggplot2_3.4.0" in search paths
library_version("ggplot2", "3.4.0")
}

}
\seealso{
\code{\link[base]{.libPaths}}
}
\author{
Simon Ress
}
\keyword{Packages}
\keyword{Paths}
\keyword{Search}
\keyword{Updates}
\keyword{for}
