% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_reqirements_file.R
\name{create_reqirements_file}
\alias{create_reqirements_file}
\title{Create an draft requirements-file}
\usage{
create_reqirements_file(
  draft = TRUE,
  req.file.path = getwd(),
  req.file.name = NULL,
  lists = TRUE,
  script.paths = NULL,
  patterns = c("library\\\\(([^,)]+)", "require\\\\(([^,)]+)",
    "library_version\\\\(([^,)]+)"),
  lib.path = NULL,
  ask.for.missing.versions = TRUE
)
}
\arguments{
\item{draft}{(bool): [TRUE] Creating a draft with the required structure of a requirements-file. [FALSE] Building requirements file base on r-scripts within 'script.path' and packages in 'lib.path'.}

\item{req.file.path}{(chr value): Path in which to create the requirements file}

\item{req.file.name}{(chr value | NULL): Name of the requirements-file or NULL: name the file "requirements_<date>.txt"}

\item{lists}{(bool): Insert a list structure into the file? TRUE=yes, FALSE=no}

\item{patterns}{(chr vector): \href{https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html}{RegEx} to detect packages used in the r-scripts}

\item{lib.path}{(chr vector): If draft==FALSE, path(s) to the libraries the package-versions should be extracted from. NULL: Don't include package versions}

\item{ask.for.missing.versions}{(bool): [draft=F] Whether to ask user to enter every missing package version or not (a package version can be missing if the package is not installed within the provided 'lib.path').}

\item{script.path}{(chr vector): If draft==FALSE, provide a path to the r-scripts including the packages which  should be extracted from}
}
\description{
Create an draft requirements-file
}
\details{
Create an draft requirements-file which can be filled with needed packages+versions & package-lists
}
\section{Dependencies}{
 None
}

\section{Side effects}{
 Creating an draft requirements-file in the given folder
}

\section{Return}{
 None
}

\examples{
\dontrun{
#Creating a req-file in the working directory called "draft-requirements.txt", with list structure
create_reqirements_file()

#Creating a req-file in "C:/Project" called "requirements.txt", without list structure
create_reqirements_file(req.file.path="C:/Project", req.file.name="requirements.txt", lists=FALSE)
}

}
\author{
Simon Ress
}
\keyword{create}
\keyword{requirements-file}
