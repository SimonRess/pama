#' Get the dependencies of an package on CRAN
#' @section Dependencies:
#' - find_package_version_on_cran()
#'
#' @description Extracts all dependencies (package names and versions) of the giving version of the package
#' @param package (chr value): Name of the package to install (e.g. "ggplot2")
#' @param version  (chr value): Version of the package to install (e.g. "3.4.0")
#' @param cran.mirror (chr value): Main url of the cran mirror to use (e.g. "https://cloud.r-project.org/")
#' @param archiv.path (chr value): URL-path to the archive of the cran mirror to use (e.g. "src/contrib/Archive/")
#' @param main.path (chr value): URL-path to the pages main page of the cran mirror to use (e.g. "src/contrib/"")
#' @param search.for.cran.name (bool): Should CRAN be searched for other name-structures of the required version?
#' @param check.in.lib (bool): FALSE: Download the package from <cran> and extract dependencies from the "DESCRIPTION" file within the downloaded folder. TRUE: Use the "DESCRIPTION" file within the package discoverable in .libPaths()
#' e.g. 0.1.10 to 0.1-1 see \code{\link[pama]{find_package_version_on_cran}}
#' TRUE=yes, FALSE=no
#'
#' @section Side effects: None
#' @section Return: List of lists with structure c(<package_name>, <version>)
#' @export
#'
#' @note See all available CRAN Packages by Name here: https://cran.r-project.org/web/packages/available_packages_by_name.html
#'
#' @examples
#' \dontrun{
#'
#' #package-VERSION does not exist -> returns "version-error"
#' get_dependencies("ggmap","2.6.0" )
#'
#' # package-VERSION does not exist -> returns "version-error"
#' get_dependencies("ggplot2","26.6.6" )
#'
#' # package-NAME does not exist -> returns "package-name-error"
#' get_dependencies("ggmapAAA","3.0.0" )
#'
#' # package-NAME does not exist -> returns "package-name-error"
#' get_dependencies("ggplot222","1.0.0" )
#'
#' # package version in archive (-> https://cran.r-project.org/src/contrib/Archive/ggplot2/)
#' get_dependencies("ggplot2","3.1.1" )
#'
#' # current version, date: 2023.03.08 (on main page -> https://cran.r-project.org/web/packages/ggplot2/index.html)
#' get_dependencies("ggplot2","3.4.1" )
#'
#' # package version in archive (->https://cran.r-project.org/src/contrib/Archive/ggmap/)
#' get_dependencies("ggmap","3.0.0" )
#'
#' # current version, date: 2023.03.08 (on main page -> https://cloud.r-project.org/web/packages/ggmap/)
#' get_dependencies("ggmap","3.0.1" )
#' }
#'
#' @author Simon Ress

get_dependencies <- function(package, version,
                             cran.mirror = "https://cloud.r-project.org/",
                             archiv.path = "src/contrib/Archive/",
                             main.path = "src/contrib/",
                             search.for.cran.name = TRUE,
                             check.in.lib = FALSE) {


  #Check format of args
    if(!is.character(package)) {stop(paste0("'package = ", package, "' is not of type <character>. Please provide a character value (e.g. 'ggplot2')!"))}
    if(!length(package)==1) {stop(paste0("'package = ", package, "' is not a single character value. Please provide a single character value (e.g. 'ggplot2')!"))}

    if(!is.character(version)) {stop(paste0("'version = ", version, "' is not of type <character>. Please provide a character value (e.g. '3.1.0')!"))}
    if(!length(version)==1) {stop(paste0("'version = ", version, "' is not a single character value. Please provide a single character value (e.g. '3.1.0')!"))}

    if(!is.character(cran.mirror)) {stop(paste0("'cran.mirror = ", cran.mirror, "' is not of type <character>. Please provide a character value!"))}
    if(!length(cran.mirror)==1) {stop(paste0("'cran.mirror = ", cran.mirror, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(archiv.path)) {stop(paste0("'archiv.path = ", archiv.path, "' is not of type <character>. Please provide a character value!"))}
    if(!length(archiv.path)==1) {stop(paste0("'archiv.path = ", archiv.path, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(main.path)) {stop(paste0("'main.path = ", main.path, "' is not of type <character>. Please provide a character value!"))}
    if(!length(main.path)==1) {stop(paste0("'main.path = ", main.path, "' is not a single character value. Please provide a single character value!"))}

    if(!is.logical(search.for.cran.name)) {stop(paste0("'search.for.cran.name = ", search.for.cran.name, "' is not of type <bool>. Please provide a boolean!"))}


  #Extract Dependencies from CRAN
    if(!check.in.lib){
      #Create package.url & package.install.path
      .out = find_package_version_on_cran(package = package, version = version,
                                          cran.mirror=cran.mirror,
                                          archiv.path=archiv.path,
                                          main.path=main.path)
      package.url = .out[1]
      version = .out[2]

      #Print info
        cat("----", "\n")
        cat("Package '", package, "' (version: ",version, ") was found on: ", package.url, "\n", sep="")
        cat("----", "\n")


      # Download the package archive
      tmp_file <- tempfile()
      utils::download.file(package.url, destfile = tmp_file, quiet=T)

      # Extract the package archive
      tmp_dir <- tempdir()
      utils::untar(tmp_file, exdir = tmp_dir)

      # Read the DESCRIPTION file
      description_file <- file.path(tmp_dir, package, "DESCRIPTION")
      description <- read.dcf(description_file)
    }


  #Extract Dependencies from local .libPaths()
    if(check.in.lib){
     tmp_dir = .libPaths()[grepl(paste0(package,"_",version), .libPaths())]
     if(identical(tmp_dir, character(0))) stop(paste0("Package ", paste0(package,"x",version), " not found in .libPaths()! Verify that the package is installed and adjust modify_libPaths() if necessary."))
    }


  # Read the DESCRIPTION file
  description_file <- file.path(tmp_dir, package, "DESCRIPTION")
  description <- read.dcf(description_file)

  #Create empty obj.
  dep.name = vector()
  dep.version = vector()

  #Check if there is an $Depends list
  if(!is.null(as.data.frame(description)$Depends)){
    #Extract required R-version
    req.r.version = strsplit(as.data.frame(description)$Depends, "\\(")[[1]][2]
    req.r.version = strsplit(req.r.version, ")")[[1]][1]
    req.r.version = gsub("\\)|\\=|>|<| |\n", "", req.r.version)

    #Extract package dependencies
    Imports = strsplit(as.data.frame(description)$Depends, ",")[[1]][-1]
    Imports = gsub(" |\n", "", Imports)
    dep.n =  unlist(lapply(strsplit(Imports, "\\("), \(x) x[1]))
    dep.name = c(dep.name, dep.n)
    dep.v = unlist(lapply(strsplit(Imports, "\\("), \(x) x[2]))
    dep.v = gsub("\\)|\\=|>|<| |\n", "", dep.v)
    dep.version = c(dep.version, dep.v)
      rm(dep.n, dep.v)

  } else req.r.version  = "0.0.0"

  #Check if there is an $Imports or $LinkingTo list
  if(!is.null(as.data.frame(description)$Imports) | !is.null(as.data.frame(description)$LinkingTo)){
    #Extract required packages + versions
    Imports = vector()
    if(!is.null(as.data.frame(description)$Imports)) Imports = c(Imports, strsplit(as.data.frame(description)$Imports, ",")[[1]])
    if(!is.null(as.data.frame(description)$LinkingTo)) Imports = c(Imports, strsplit(as.data.frame(description)$LinkingTo, ",")[[1]])
      #Imports = strsplit(Imports, ",")[[1]]
      Imports = gsub(" |\n", "", Imports)
      dep.n = unlist(lapply(strsplit(Imports, "\\("), \(x) x[1]))
      dep.name = c(dep.name, dep.n)
      dep.v = unlist(lapply(strsplit(Imports, "\\("), \(x) x[2]))
      dep.v = gsub("\\)|\\=|>|<| |\n", "", dep.v)
      dep.version = c(dep.version, dep.v)
        rm(dep.n, dep.v)
    } #else{
      #dependencies = NA
    #}

  #Build return
    dependencies = data.frame(name = dep.name, version = dep.version)
    dependencies = dependencies[!is.na(dependencies$name),] #drop rows with NA on "names"-column
    dependencies = dependencies[dependencies$name!="R",] #drop rows with R-version (seems to be an old format)


  #Close all connections
  suppressWarnings(try(closeAllConnections(),silent=T))
  # showConnections(all = TRUE)

  # Return the required R-version and dependencies
  out = list("R-version" = data.frame(name="R",version=req.r.version), "Packages" = dependencies)
  return(out)
}

#test
#All available CRAN Packages by Name: https://cran.r-project.org/web/packages/available_packages_by_name.html

# get_dependencies("ggmap","2.6.0" ) # package-VERSION does not exist -> works
# get_dependencies("ggplot2","26.6.6" ) # package-VERSION does not exist -> works
#
# get_dependencies("ggmapAAA","3.0.0" ) # package-NAME does not exist -> works
# get_dependencies("ggplot222","1.0.0" ) # package-NAME does not exist -> works
#
#
# get_dependencies("ggplot2","3.1.1" ) # package version in archive -> works // https://cran.r-project.org/src/contrib/Archive/ggplot2/
# get_dependencies("ggplot2","3.4.1" ) # current (2023.03.08)  package version (on main page) -> works // https://cran.r-project.org/web/packages/ggplot2/index.html
#
# get_dependencies("ggmap","3.0.0" ) # package version in archive -> works // https://cran.r-project.org/src/contrib/Archive/ggmap/
# get_dependencies("ggmap","3.0.1" ) # current (2023.03.08) package version (on main page) -> works // https://cloud.r-project.org/web/packages/ggmap/
