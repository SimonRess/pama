#' Get the dependencies of an package on CRAN
#' @section Dependencies: None
#'
#' @description Extracts all dependencies (package names and versions) of the giving version of the package
#' @param package sad
#' @param version  sd
#' @param cran.mirror vv
#' @param archiv.path fdf
#' @param main.path xc
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

get_dependencies <- function(package, version, cran.mirror = "https://cloud.r-project.org/", archiv.path = "src/contrib/Archive/", main.path = "src/contrib/") {
  if(!is.character(package)) {warning("Provide the package name as string (e.g. 'ggplot2')"); stop()}
  if(!is.character(version)) {warning("Provide the version name as string (e.g. '3.1.0')"); stop()}


  # #Check whether version is in archive or on main package-page
  # #Construct URLs (1. archive / 2. main package page)
  # archive.url = paste0(cran.mirror, archiv.path, package, "/", package, "_", version, ".tar.gz")
  # main.page.url = paste0(cran.mirror, main.path, package, "_", version, ".tar.gz") # don't look into "/Archive/" -> get newest version
  # #https://cloud.r-project.org/src/contrib/ggmap_3.0.1.tar.gz
  #
  # #Check if constructed URL is correct -> Find correct url
  # cat("--------------------", "\n")
  #   # 1. check archive
  #   check = suppressWarnings(try(readLines(archive.url), silent=T)) # open.connection(url(...),open="rt",timeout=10)
  #   if(!inherits(check, "try-error")) {
  #     package.url = package.url
  #   } else {
  #     # 2. check main page (NOT archive)
  #     check = suppressWarnings(try(readLines(main.page.url), silent=T)) # open.connection(url(...),open="rt",timeout=10)
  #     if(!inherits(check, "try-error")) {
  #         package.url = main.page.url
  #       } else {
  #         # 3. try to change version structure e.g. from 0.1.10 to 0.1-1
  #           #e.g. dplyr_0.8.0 (https://cloud.r-project.org/src/contrib/Archive/dplyr/dplyr_0.8.0.tar.gz) depends on plogr 0.1-1
  #           #but there is only an version plogr 0.1.10 (https://cloud.r-project.org/src/contrib/Archive/plogr/, https://cloud.r-project.org/src/contrib/)
  #           version.mod = sub("0([^0]*)$", "\\1",sub(".([^.]*)$", "-\\1", version)) #change 0.1.10 to 0.1-1
  #           new.package.url = paste0(cran.mirror, "src/contrib/Archive/", package, "/", package, "_", version.mod, ".tar.gz")
  #           check = suppressWarnings(try(readLines(new.package.url),silent = T)) # open.connection(url(),open="rt",timeout=t
  #           #suppressWarnings(try(close.connection(url(new.package.url)),silent=T))
  #           if(!inherits(check, "try-error")) {
  #             package.url = new.package.url
  #             cat("---")
  #             cat("Version", version, "is named", version.mod, "in CRAN. This version will be installed!")
  #             cat("---")
  #             version = version.mod
  #           } else {
  #           #If package-version was nowhere found:
  #               cat("Error!!! Package ", package, " (version: ",version, ") was not found in: \n", sep="")
  #               cat("- (archive)", package.url, "\n", sep="")
  #               cat("- (newest)", new.package.url, "\n", sep="")
  #               cat("- (modified version)", new.package.url2, "\n", sep="")
  #               cat("----")
  #
  #               #Check if package exists
  #               check = suppressWarnings(try(readLines(paste0(cran.mirror, "web/packages/", package)), silent=T))
  #               if(inherits(check, "try-error")) {
  #                 #Messages
  #                 cat("No package by name '", package,"' found!", "\n", sep="")
  #                 cat("(INFO) Find a list of all packages on CRAN here: https://cran.r-project.org/web/packages/available_packages_by_name.html", "\n")
  #                 cat("---------------", "\n")
  #
  #                 stop(paste0("package-name-error, ", "SOLUTION: 1. Check the spelling of the package name. 2.Install the required package by hand: ", package, "_", version))
  #                 #return("package-name-error")
  #               } else {
  #               #Info-Message about existing versions
  #                 #scrape versions in archive
  #                 archive.versions = readLines(paste0("https://cloud.r-project.org/src/contrib/Archive/", package))
  #                 archive.versions = archive.versions[(grep("Parent Directory", archive.versions)+1):(grep("<hr></pre>", archive.versions)-1)]
  #                 archive.versions = sapply(strsplit(archive.versions, '<a href=\"'), \(x) strsplit(x[2], '.tar.gz\">')[[1]][1])
  #                 #scrape newest version
  #                 newest.version = readLines(paste0(cran.mirror, "web/packages/", package))
  #                 newest.version = newest.version[grep("<td>Version:</td>",newest.version)+1]
  #                 newest.version = gsub("<td>|</td>", "", newest.version)
  #                 newest.version = paste0(package, "_", newest.version)
  #
  #                 #Messages
  #                 cat("(INFO) Available versions of '", package, "':","\n", sep="")
  #                 cat("- In archive:", paste(archive.versions, collapse = ", "), "\n")
  #                 cat("- Newest version:", newest.version, "\n")
  #                 cat("------", "\n")
  #
  #                 stop(paste0("version-error, ", "SOLUTION: 1. Check the spelling of the package name. 2.Install the required package by hand: ", package, "_", version))
  #                 #return("version-error")
  #               }
  #           }
  #       }
  # }
  #
  #
  #
  #   if(inherits(check, "try-error")) {
  #     cat("Error!!! Package ", package, " (version ",version, ") was not found here: \n", "- Archive: ", archive.url, "\n", "- Main page: ", main.page.url, "\n", sep="")
  #     cat("---","\n")
  #
  #     #Check if package exists
  #     check = suppressWarnings(try(readLines(paste0(cran.mirror, "web/packages/", package)), silent=T))
  #     if(inherits(check, "try-error")) {
  #       #Messages
  #       cat("No package by name '", package,"' found!", "\n", sep="")
  #       cat("(INFO) Find a list of all packages on CRAN here: https://cran.r-project.org/web/packages/available_packages_by_name.html", "\n")
  #       cat("------------------------------------------", "\n")
  #
  #       stop(paste0("package-name-error, ", "SOLUTION: Install the required package by hand: ", package, "_", version))
  #       #return("package-name-error")
  #     } else {
  #       #scrape versions in archive
  #       archive.versions = readLines(paste0("https://cloud.r-project.org/src/contrib/Archive/", package))
  #       archive.versions = archive.versions[(grep("Parent Directory", archive.versions)+1):(grep("<hr></pre>", archive.versions)-1)]
  #       archive.versions = sapply(strsplit(archive.versions, '<a href=\"'), \(x) strsplit(x[2], '.tar.gz\">')[[1]][1])
  #       #scrape newest version
  #       newest.version = readLines(paste0(cran.mirror, "web/packages/", package))
  #       newest.version = newest.version[grep("<td>Version:</td>",newest.version)+1]
  #       newest.version = gsub("<td>|</td>", "", newest.version)
  #       newest.version = paste0(package, "_", newest.version)
  #
  #       #Messages
  #       cat("(INFO) Available versions of '", package, "':","\n", sep="")
  #       cat("- In archive:", paste(archive.versions, collapse = ", "), "\n")
  #       cat("- Newest version:", newest.version, "\n")
  #       #suppressWarnings(try(closeAllConnections(),silent=T))
  #       cat("------------------------------------------", "\n")
  #
  #       stop(paste0("version-error, ", "SOLUTION: Install the required package by hand: ", package, "_", version))
  #       #return("version-error")
  #     }
  #
  #
  #   }
  #   package.url = main.page.url
  #   cat("Package '", package, "' (version: ",version, ") was found on: ", package.url, "\n", sep="")
  # } else if(!inherits(check, "try-error")){
  #   package.url = archive.url
  #   cat("Package '", package, "' (version: ",version, ") was found on: ", package.url, "\n", sep="")
  # }


  #Create package.url & package.install.path
    package.url = find_package_version_on_cran(package = package, version = version)
    package.install.path = paste0(lib.install.path,"/", package, "_", version)

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

      dependencies = data.frame(name = dep.name, version = dep.version)
      dependencies = dependencies[!is.na(dependencies$name),] #drop rows with NA on "names"-column
    } else{
      dependencies = NA
    }



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
