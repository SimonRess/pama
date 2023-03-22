#' Find the URL to a specific package version on CRAN
#' @section Dependencies: None
#'
#' @description
#' Finding the URL to a specific package version on CRAN. Therefore searching the archive and the main page of the given cran instance.
#' Then trying to changing the version name structure by replacing the last "." by "-" and deleting trailing zeros.
#'
#' (e.g. "0.1.10" to "0.1-1", because \href{https://cloud.r-project.org/src/contrib/Archive/dplyr/}{dplyr_0.8.0} requires "plogr_0.1.10"
#' but in \href{https://cloud.r-project.org/src/contrib/Archive/plogr/}{plogrs archive} this version is named "plogr_0.1-1]
#' @param package (chr vector): Name of the package to install (e.g. "ggplot2")
#' @param version (chr vector): Version of the package to install (e.g. "3.4.0")
#' @param cran.mirror (chr vector): Main url of the cran mirror to use (e.g. "https://cloud.r-project.org/")
#' @param archiv.path (chr vector): URL-path to the archive of the cran mirror to use (e.g. "src/contrib/Archive/")
#' @param main.path (chr vector): URL-path to the pages main page of the cran mirror to use (e.g. "src/contrib/"")
#'
#' @section Side effects: None
#' @return (chr vector):
#' [1] (chr) URL of the package version or an error message
#'
#' [2] (chr) Version of the package (sometimes the version changes, therefore an output is needed)
#'
#' @export
#'
#' @note See all available CRAN Packages by Name here: https://cran.r-project.org/web/packages/available_packages_by_name.html
#'
#' @keywords finding package-versions on CRAN
#' @examples
#' \dontrun{
#' .out = find_package_version_on_cran(package = package, version = version)
#' package.url = .out[1]
#' version = .out[2]
#' }

find_package_version_on_cran = function(package, version, cran.mirror = "https://cloud.r-project.org/", archiv.path = "src/contrib/Archive/", main.path = "src/contrib/") {
#Check whether version is in archive or on main package-page

  #Construct URLs (1. archive / 2. main package page)
  archive.url = paste0(cran.mirror, archiv.path, package, "/", package, "_", version, ".tar.gz")
  main.page.url = paste0(cran.mirror, main.path, package, "_", version, ".tar.gz") # don't look into "/Archive/" -> get newest version
  .package.url = "" # see hidden objects by ls(all.names = TRUE)
  .version = version
    #https://cloud.r-project.org/src/contrib/ggmap_3.0.1.tar.gz

  #Check if constructed URL is correct -> Find correct url
  cat("--------------------", "\n")
    # 1. check archive
    check = suppressWarnings(try(readLines(archive.url), silent=T)) # open.connection(url(...),open="rt",timeout=10)
    if(!inherits(check, "try-error")) {
      .package.url = archive.url
      #Stop&Return
        return(c(.package.url, .version))

    } else {
      # 2. check main page (NOT archive)
      check = suppressWarnings(try(readLines(main.page.url), silent=T)) # open.connection(url(...),open="rt",timeout=10)

      if(!inherits(check, "try-error")) {
          .package.url = main.page.url
          #Stop&Return
            return(c(.package.url, .version))

        } else {
          # 3. try to change version structure e.g. from 0.1.10 to 0.1-1
            #e.g. dplyr_0.8.0 (https://cloud.r-project.org/src/contrib/Archive/dplyr/dplyr_0.8.0.tar.gz) depends on plogr 0.1-1
            #but there is only an version plogr 0.1.10 (https://cloud.r-project.org/src/contrib/Archive/plogr/, https://cloud.r-project.org/src/contrib/)
            version.mod = sub("0([^0]*)$", "\\1",sub(".([^.]*)$", "-\\1", version)) #change 0.1.10 to 0.1-1
            new.package.url = paste0(cran.mirror, "src/contrib/Archive/", package, "/", package, "_", version.mod, ".tar.gz")
            check = suppressWarnings(try(readLines(new.package.url),silent = T)) # open.connection(url(),open="rt",timeout=t

            if(!inherits(check, "try-error")) {
              .package.url = new.package.url
              cat("---", "\n")
              cat("Version", version, "is named", version.mod, "in CRAN. This version will be used!", "\n")
              cat("---", "\n")
              #version <<- version.mod # NOT WORKING, BINDING IS LOCKED!  #search in parent envS for an existing obj. and assign value to it (otherwise create obj. in the global environment)
              #.version = version.mod
              #Stop&Return
                return(c(.package.url, version.mod))

            } else {
            #If package-version was nowhere found:
                cat("Error!!! Package ", package, " (version: ",version, ") was not found in: \n", sep="")
                cat("- (archive)", archive.url, "\n", sep="")
                cat("- (newest)", main.page.url, "\n", sep="")
                cat("- (modified version)", new.package.url, "\n", sep="")
                cat("---", "\n")

                #Check if package exists
                check = suppressWarnings(try(readLines(paste0(cran.mirror, "web/packages/", package)), silent=T))

                if(inherits(check, "try-error")) {
                  #Messages
                  cat("No package by name '", package,"' found!", "\n", sep="")
                  cat("(INFO) Find a list of all packages on CRAN here: https://cran.r-project.org/web/packages/available_packages_by_name.html", "\n")
                  cat("---------------", "\n")

                  stop(paste0("package-name-error, ", "SOLUTION: 1. Check the spelling of the package name. 2.Install the required package by hand: ", package, "_", version))
                  #return("package-name-error")

                } else {
                #Info-Message about existing versions
                  #scrape versions in archive
                  archive.versions = readLines(paste0("https://cloud.r-project.org/src/contrib/Archive/", package))
                  archive.versions = archive.versions[(grep("Parent Directory", archive.versions)+1):(grep("<hr></pre>", archive.versions)-1)]
                  archive.versions = sapply(strsplit(archive.versions, '<a href=\"'), \(x) strsplit(x[2], '.tar.gz\">')[[1]][1])
                  #scrape newest version
                  newest.version = readLines(paste0(cran.mirror, "web/packages/", package))
                  newest.version = newest.version[grep("<td>Version:</td>",newest.version)+1]
                  newest.version = gsub("<td>|</td>", "", newest.version)
                  newest.version = paste0(package, "_", newest.version)

                  #Messages
                  cat("(INFO) Available versions of '", package, "':","\n", sep="")
                  cat("- In archive:", paste(archive.versions, collapse = ", "), "\n")
                  cat("- Newest version:", newest.version, "\n")
                  cat("------", "\n")

                  stop(paste0("version-error, ", "SOLUTION: 1. Check the spelling of the package name. 2.Install the required package by hand: ", package, "_", version))
                  #return("version-error")
                }
            }
        }
  }

}
