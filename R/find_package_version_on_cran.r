#' Find the URL to a specific package version on CRAN
#' @section Dependencies: None
#'
#' @description
#' Finding the URL to a specific package version on CRAN. Therefore searching the archive and the main page of the given cran instance.
#' Then trying to changing the version name structure by replacing the last "." by "-" and deleting trailing zeros.
#'
#' (e.g. "0.1.10" to "0.1-1", because \href{https://cloud.r-project.org/src/contrib/Archive/dplyr/}{dplyr_0.8.0} requires "plogr_0.1.10"
#' but in \href{https://cloud.r-project.org/src/contrib/Archive/plogr/}{plogrs archive} this version is named "plogr_0.1-1]
#' @param package (chr value): Name of the package to install (e.g. "ggplot2")
#' @param version (chr value): Version of the package to install (e.g. "3.4.0")
#' @param cran.mirror (chr value): Main url of the cran mirror to use (e.g. "https://cloud.r-project.org/")
#' @param archiv.path (chr value): URL-path to the archive of the cran mirror to use (e.g. "src/contrib/Archive/")
#' @param main.path (chr value): URL-path to the pages main page of the cran mirror to use (e.g. "src/contrib/"")
#' @param repo (chr value): Which type of repo is used? This information is utilized when building the URL,
#' because each repo has its own file structure. Currently supported: "cran" & "nexus"
#'
#' @section Side effects: None
#' @return (chr vector):
#' return[1] (chr): URL of the package version or an error message
#'
#' return[2] (chr): Version of the package (sometimes the version changes, therefore an output is needed)
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
#'

find_package_version_on_cran = function(package, version,
                                        cran.mirror = "https://cloud.r-project.org/",
                                        archiv.path = "src/contrib/Archive/",
                                        main.path = "src/contrib/",
                                        repo="cran") {


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

    if(!is.character(repo)) {stop(paste0("repo' is not an character value. Provide an character value!"))}
    if(!length(repo)==1) {stop(paste0("repo' is not of length==1. Provide exactly one package name!"))}


#If no version is transmitted -> Use the newest version
  if(is.na(version) | version=="NA"){
    cat("No version was stated (version = NA)!", "\n")
    #scrape newest version
      if(repo=="cran"){
        newest.version = readLines(paste0(cran.mirror, "web/packages/", package))
        newest.version = newest.version[grep("<td>Version:</td>",newest.version)+1]
        newest.version = gsub("<td>|</td>", "", newest.version)
        #newest.version = paste0(package, "_", newest.version)
      }
      if(repo=="nexus"){
        stop("No <version> provided! Please provide a version (nexus error)!")
      }

    #replace version==NA by the newest version
      version = newest.version
      .version = newest.version
      cat("Therefore, the newest version (", version, ") of the package will be used.", "\n", sep="")

  }



#Check whether version is in archive or on main package-page
  #Construct URLs (1. archive / 2. main package page)
  #Replaces by build_package_url(): #archive.url = paste0(cran.mirror, archiv.path, package, "/", package, "_", version, ".tar.gz")
  archive.url = build_package_url(.main=FALSE, .package=package, .version=version)
  #Replaces by build_package_url(): #main.page.url = paste0(cran.mirror, main.path, package, "_", version, ".tar.gz") # don't look into "/Archive/" -> get newest version
  main.page.url = build_package_url(.main=TRUE, .package=package, .version=version) # don't look into "/Archive/" -> get newest version
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

        } else { #Check version.mod in Archive
          # 3. try to change version structure e.g. from 0.1.10 to 0.1-1
            #e.g. dplyr_0.8.0 (https://cloud.r-project.org/src/contrib/Archive/dplyr/dplyr_0.8.0.tar.gz) depends on plogr 0.1-1
            #but there is only an version plogr 0.1.10 (https://cloud.r-project.org/src/contrib/Archive/plogr/, https://cloud.r-project.org/src/contrib/)
            version.mod = sub("0([^0]*)$", "\\1",sub(".([^.]*)$", "-\\1", version)) #change 0.1.10 to 0.1-1
            #Replaces by build_package_url(): new.package.url = paste0(cran.mirror, archiv.path, package, "/", package, "_", version.mod, ".tar.gz")
            new.package.url = build_package_url(.main=FALSE, .package=package, .version=version.mod)
            check = suppressWarnings(try(readLines(new.package.url),silent = T)) # open.connection(url(),open="rt",timeout=t

            if(inherits(check, "try-error")) { #Check version.mod in main
              #Replaces by build_package_url(): # new.package.url = paste0(cran.mirror, main.path, package, "_", version.mod, ".tar.gz")
              new.package.url = build_package_url(.main=TRUE, .package=package, .version=version.mod)
              check = suppressWarnings(try(readLines(new.package.url),silent = T)) # open.connection(url(),open="rt",timeout=t
            }

            if(!inherits(check, "try-error")) { #if version found, return it
              .package.url = new.package.url
              cat("---", "\n")
              cat("Version", version, "is named", version.mod, "in CRAN. This version will be used!", "\n")
              #version <<- version.mod # NOT WORKING, BINDING IS LOCKED!  #search in parent envS for an existing obj. and assign value to it (otherwise create obj. in the global environment)
              #.version = version.mod
              #Stop&Return
                return(c(.package.url, version.mod))

            } else{ #Check version.mod2 in Archive
              version.mod2 = paste0(version,".0") #change 0.1. to 0.1.0

              #Replaces by build_package_url(): #new.package.url = paste0(cran.mirror, archiv.path, package, "/", package, "_", version.mod2, ".tar.gz")
              new.package.url = build_package_url(.main=FALSE, .package=package, .version=version.mod2)
              check = suppressWarnings(try(readLines(new.package.url),silent = T)) # open.connection(url(),open="rt",timeout=t

              if(inherits(check, "try-error")) { #Check version.mod2 in main
                #Replaces by build_package_url(): #new.package.url = paste0(cran.mirror, main.path, package, "_", version.mod2, ".tar.gz")
                new.package.url = build_package_url(.main=TRUE, .package=package, .version=version.mod2)
                check = suppressWarnings(try(readLines(new.package.url),silent = T)) # open.connection(url(),open="rt",timeout=t
              }

              if(!inherits(check, "try-error")) { #if version found, return it
                .package.url = new.package.url
                cat("---", "\n")
                cat("Version", version, "is named", version.mod2, "in CRAN. This version will be used!", "\n")
                #version <<- version.mod # NOT WORKING, BINDING IS LOCKED!  #search in parent envS for an existing obj. and assign value to it (otherwise create obj. in the global environment)
                #.version = version.mod
                #Stop&Return
                  return(c(.package.url, version.mod2))

              } else { #If package-version was nowhere found:
                  cat("Error!!! Package ", package, " (version: ",version, ") was not found in: \n", sep="")
                  cat("- (archive) ", archive.url, "\n", sep="")
                  cat("- (newest) ", main.page.url, "\n", sep="")
                  cat("- (modified version) ", new.package.url, "\n", sep="")
                  cat("---", "\n")

                  #Check if package exists on CRAN
                  #check = suppressWarnings(try(readLines(paste0(cran.mirror, "web/packages/", package)), silent=T))
                  check = suppressWarnings(try(readLines(build_package_url(.overview=T, .package=package, .version=version)), silent=T))

                  #If Package doesn't exists...
                  if(inherits(check, "try-error")) {
                    #Messages
                    cat("No package by name '", package,"' found!", "\n", sep="")
                    cat("(INFO) Find a list of all packages on CRAN here: https://cran.r-project.org/web/packages/available_packages_by_name.html", "\n")
                    cat("---------------", "\n")

                    stop(paste0("package-name-error, ", "SOLUTION: 1. Check the spelling of the package name. 2.Install the required package by hand: ", package, "_", version))
                    #return("package-name-error")

                  #If Package do exists...
                  } else {
                  #Info-Message about existing versions
                    #scrape versions in archive
                    if(repo=="cran"){
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

                      exit = FALSE
                      while(exit==FALSE) {
                        user.version = readline(prompt = "Name the ONE version NUMBER to be installed (e.g 1.2.0-1 or 1.4.7; tip: choose the one closest to the one you need): ")
                        #Create url base of choice: archive or main (newest version)
                          if(paste0(package,"_",user.version) %in% archive.versions) user.package.url = build_package_url(.main=FALSE, .package=package, .version=user.version) # paste0(cran.mirror, archiv.path, package, "/", package,"_",user.version, ".tar.gz")
                          if(paste0(package,"_",user.version) %in% newest.version) user.package.url = build_package_url(.main=TRUE, .package=package, .version=user.version) # paste0(cran.mirror, main.path, package,"_",user.version, ".tar.gz")

                        #Correct input -> exit while loop?
                        if(paste0(package,"_",user.version) %in% archive.versions | paste0(package,"_",user.version) %in% newest.version) exit=TRUE
                        else cat("'", user.version, "' is not a version number (e.g. 3.1-162) from the archive nor the newest version. Choose one of the available versions!", "\n", sep="")
                      }

                      check = suppressWarnings(try(readLines(user.package.url),silent = T)) # open.connection(url(),open="rt",timeout=t

                      if(!inherits(check, "try-error")) {
                        .package.url = user.package.url
                        cat("---", "\n")
                        cat("Version", version, "is required but is not found / could not be installed. Instead version", user.version, "will be used!", "\n")
                        #Stop&Return
                        return(c(.package.url, user.version))
                      }

                      else{
                        stop(paste0("version-error, ", "SOLUTION: 1. Check the spelling of the package name. 2.Install the required package by hand: ", package, "_", version))
                        #return("version-error")
                      }
                    }
                    if(repo=="nexus"){
                      stop(paste0("Check ", build_package_url(.overview=T, .package=package, .version=version), " for available versions!"))
                    }



                  }
              }
            }
        }
  }

}
