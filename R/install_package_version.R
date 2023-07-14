#' Installing a specific version of a package (keep other versions!)
#' @section Dependencies:
#' - find_package_version_on_cran()
#' - get_installed_packages() [<- update_packages_search_path()]
#'
#' - get_dependencies()
#'
#' - update_packages_search_path()
#'
#' @description !!! Recursive function !!!
#' Steps:
#' 1. Trying to find the requested package version on CRAN
#'   1.1 ... if found: download .tar.gz-file
#'   1.2 ... else: check which versions are available and ask user to choose one
#' 2. Extracting dependencies of the package version & compare with installed package versions
#' 3. Checking whether all requirements are fulfilled (no check for exact match, just for min. version)
#'   3.1 ... if true: install package version
#'   3.2 ... else: (recursive!) use install_package_version() for every unsatisfied requirement.
#'     If version of the required package is 'NA', install newest version
#' 4. After installation load packages once to ensure functionality
#' @param package (chr value): Name of the package to install (e.g. "ggplot2")
#' @param version  (chr value): Version of the package to install (e.g. "3.4.0")
#' @param lib.install.path (chr value): Folder in which to install the packages
#' @param use.only.lib.install.path (bool): Only check in <lib.install.path> for installed packages and dependencies
#'
#' @param cran.mirror (chr value): Main url of the cran mirror to use (e.g. "https://cloud.r-project.org/")
#' @param archiv.path (chr value): URL-path to the archive of the cran mirror to use (e.g. "src/contrib/Archive/")
#' @param main.path (chr value): URL-path to the pages main page of the cran mirror to use (e.g. "src/contrib/"")
#' @param repo (chr value): Which type of repo is used? This information is utilized when building the URL,
#' because each repo has its own file structure. Currently supported: "cran" & "nexus"
#'
#' @param auto.update.version.in.files (bool):
#' If TRUE the version of a package in the installed files will be changed to the required version. This only happens if
#' it's the same version but the structure of the version name differs e.g. 0.1.10 to 0.1-1 see \code{\link[pama]{find_package_version_on_cran}}
#' When FALSE nothing happens
#'
#' @section Side effects: Installation of the package & adding package location to the search paths
#' @section Return: TRUE (successful installation) or FALSE (UNsuccessful installation)
#' @export
#'
#' @note See all available CRAN Packages by Name here: https://cran.r-project.org/web/packages/available_packages_by_name.html
#'
#' @section test:
#' ```{r lorem}
#' 1+1
#' ```
#'
#' @seealso [pama::setupLib()], `browseVignettes("pama")`, `help(package = "pama")`
#'
#' @keywords installing package-versions
#' @examples
#' \dontrun{
#' sessionInfo()
#' install_package_version("ggplot2", "3.4.0")
#' sessionInfo()
#' library_version("ggplot2", "3.4.0")
#' sessionInfo()
#' detach(paste0("package:","ggplot2"), character.only = TRUE) # character.only = TRUE <- needed when paste0() or object used
#' }
#'
#' @author Simon Ress


install_package_version = function(package,
                                   version,
                                   lib.install.path=.libPaths()[1],
                                   use.only.lib.install.path=T,
                                   cran.mirror = "https://cloud.r-project.org/",
                                   archiv.path = "src/contrib/Archive/",
                                   main.path = "src/contrib/",
                                   repo="cran",
                                   auto.update.version.in.files = TRUE) {

    #Check format of args
    if(!is.character(package)) {stop(paste0("'package = ", package, "' is not of type <character>. Please provide a character value! (e.g. 'ggplot2')"))}
    if(!length(package)==1) {stop(paste0("'package = ", package, "' is not a single character value. Please provide a single character value! (e.g. 'ggplot2')"))}

    if(!is.character(version)) {stop(paste0("'version = ", version, "' is not of type <character>. Please provide a character value! (e.g. '3.1.0')"))}
    if(!length(version)==1) {stop(paste0("'version = ", version, "' is not a single character value. Please provide a single character value! (e.g. '3.1.0')"))}

    if(!is.character(lib.install.path)) {stop(paste0("'lib.install.path = ", lib.install.path, "' is not of type <character>. Please provide a character value!"))}

    if(!is.logical(use.only.lib.install.path)) {stop(paste0("'use.only.lib.install.path = ", use.only.lib.install.path, "' is not of type <bool>. Please provide a boolean!"))}

    if(!is.character(cran.mirror)) {stop(paste0("'cran.mirror = ", cran.mirror, "' is not of type <character>. Please provide a character value!"))}
    if(!length(cran.mirror)==1) {stop(paste0("'cran.mirror = ", cran.mirror, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(archiv.path)) {stop(paste0("'archiv.path = ", archiv.path, "' is not of type <character>. Please provide a character value!"))}
    if(!length(archiv.path)==1) {stop(paste0("'archiv.path = ", archiv.path, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(main.path)) {stop(paste0("'main.path = ", main.path, "' is not of type <character>. Please provide a character value!"))}
    if(!length(main.path)==1) {stop(paste0("'main.path = ", main.path, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(repo)) {stop(paste0("repo' is not an character value. Provide an character value!"))}
    if(!length(repo)==1) {stop(paste0("repo' is not of length==1. Provide exactly one package name!"))}

    if(!is.logical(auto.update.version.in.files)) {stop(paste0("'auto.update.version.in.files = ", auto.update.version.in.files, "' is not of type <bool>. Please provide a boolean!"))}
    if(!length(auto.update.version.in.files)==1) {stop(paste0("auto.update.version.in.files' is not of length==1. Provide exactly one bool!"))}


  # Change .libPaths() to <lib.install.path>
  if(use.only.lib.install.path){
    modify_libPaths(reset=lib.install.path)
    cat(".libPaths() is changed to:", .libPaths(), "\n") # lib.install.path
    cat("No other folders will be checked for libraries/dependencies!", "\n")
    cat("All installed packages and all dependencies of these packages will be installed there!", "\n")

  }


  #Some status reporting
    cat("auto.update.version.in.files: ", auto.update.version.in.files, "\n")
    cat("-------------------------------------------------------------------", "\n")
    cat("Start the Installation of package '", package, "' (version ", version, ")", "\n", sep="")

  #Create package.url & package.install.path
    if(!is.na(version) & version!="NA"){
      .out = find_package_version_on_cran(package = package, version = version,
                                          cran.mirror=cran.mirror,
                                          archiv.path=archiv.path,
                                          main.path=main.path,
                                          repo=repo) # prints: "Version 0.1.10 is named 0.1-1 in CRAN. This version will be used!"
      package.url = .out[1]
    } else {
      #Use the newest version
        #scrape newest version
          if(repo=="cran"){
            newest.version = readLines(paste0(cran.mirror, "web/packages/", package))
            newest.version = newest.version[grep("<td>Version:</td>",newest.version)+1]
            newest.version = gsub("<td>|</td>", "", newest.version)
            #newest.version = paste0(package, "_", newest.version)
          #replace version==NA by the newest version
            version = newest.version
          #continue as above
            .out = find_package_version_on_cran(package = package, version = version) # prints: "Version 0.1.10 is named 0.1-1 in CRAN. This version will be used!"
            package.url = .out[1]
          }
          if(repo=="nexus"){
            stop("No <version> provided! Please provide a version (nexus error)!")
          }

    }


    # 3 version-vars: version.required = version to install // "version" = naming folders and edit version in package-file // version.installing = download this version & check if this version is installed
    version.required = version
    version.installing = .out[2] # new version name e.g. 0.1.10 -> 0.1-1
    if(auto.update.version.in.files == TRUE) version.name = version
    if(auto.update.version.in.files == FALSE) version.name = version.installing

    package.install.path = paste0(lib.install.path,"/", package, "_", version.name)

    #update version of package if structure of version name on CRAN differs to required version
      # if(auto.update.version.in.files & version != .version) {
      #   .install.version = .version
      # } else .install.version = version


  #Print info
    cat("----", "\n")
    #cat("Package Url: ", package.url, "\n", sep="")
    cat("Package '", package, "' (version: ", version.installing, ") was found on: ", package.url, "\n", sep="")
    cat("Local package installation folder: ", package.install.path, "\n", sep="")

  #Check installed R Version
  rversion.installed = paste0(R.version$major,".",R.version$minor)

  #Check which dependencies are needed & Check whether package-name and -version exist
  # package = "ggplot2"
  # version = "3.4.0"
  # package = "vctrs"
  # version = "0.5.0"
  depends.on = get_dependencies(package=package, version=version.installing,
                                cran.mirror=cran.mirror,
                                archiv.path=archiv.path,
                                main.path=main.path,
                                repo=repo,
                                search.for.cran.name=FALSE)
  #cat("---", "\n")
  #cat("depends.on", "\n")
  #cat(unlist(depends.on[["Packages"]]), "\n")
  rversion.required = depends.on$`R-version`$version

  #Check R-VERSION
  if(rversion.installed != rversion.required) {
    cat("----", "\n")
    cat("The installed r-version does not match the required r-version (installed:",rversion.installed," != required:",rversion.required,")\n", sep="")
    #install = ""
    # while(toupper(install)!="Y" & toupper(install)!="N") {
    #   install <- readline(prompt=paste0("Do you want to install the required R-Version (",rversion.required,") now [Y/N]?: "))
    #   if(toupper(install) == "Y") cat("Not implementet yet (dependency 'installr' & 'devtools' would be needed, what is not desired). \n -> Please install R-version ",rversion.required," by your own and try install_requirements() again. \n", sep="")
    #   if(toupper(install) == "N") cat("-> Please install R-version ",rversion.required," by your own and try install_requirements() again. \n", sep="")
    # }
  }

  #Some packages don't use dependencies -> do stuff below only when dependencies are present
  if(!all(is.na(depends.on$Packages))){ # all() needed because obj is normaly a dataframe and is.na() produces therefore several outputs
    #Check already installed packages & R-verison
    already.installed = get_installed_packages()
    #keep only the highest installed version
    already.installed = merge(stats::aggregate(version ~ name, max, data = already.installed), already.installed)

    #Uninstalled but required packages or installed version too old
    m = merge(depends.on$Packages, already.installed, by="name", all.x = TRUE, all.y = FALSE, suffixes = c(".required",".installed"))
    #print(m)
    get1 = m[is.na(m$version.installed),] # required, but not installed (or not version available)
    #print(get1)
    # Not working because of data-type chr: get2 = m[!is.na(m$version.required) & !is.na(m$version.installed) & m$version.required > m$version.installed,] # required version > installed version
    get2 = m[!is.na(m$version.required) & !is.na(m$version.installed) & apply(m, 1, \(x) compareVersion(x["version.required"], x["version.installed"])==1),] # 1: required version > installed version / -1: required version < installed version

    #print(get2)
    get = rbind(get1,get2)
    #delete empty lines
    get = get[!is.na(get$name),]

    if(nrow(get)>0){
      print("-----")
      cat("Unsatisfied requirements: \n")
      #print(get)
      cat("name | version.required | version.installed", "\n")
      for(s in 1:nrow(get)){
        cat(paste(unlist(get[s,]), collapse =" | "), "\n")
      }
      #cat(unlist(get))
      cat("-----")
    } else {
        print("-----")
        cat("All requirements satisfied! \n")
        cat("name | version.required | version.installed", "\n")
        for(s in 1:nrow(m)){
          cat(paste(unlist(m[s,]), collapse =" | "), "\n")
        }
        cat("-----")
    }

    #Recursion: Invoke itself until there are no more unfulfilled preconditions, continue script with this package -> after ending the script, continue with the next "higher" package below if condition
    if(nrow(get)>0){
      for(p in 1:nrow(get)) {
        #if required package version is NA -> get newest version of this package
        if(is.na(get$name[p]) & is.na(get$version.required[p])) {
          if(repo=="cran"){
            #scrape newest version
            newest.version = readLines(paste0(cran.mirror, "web/packages/", get$name[p])) #package name from: get$name[p]
            newest.version = newest.version[grep("<td>Version:</td>",newest.version)+1]
            newest.version = gsub("<td>|</td>", "", newest.version)
            #newest.version = paste0(package, "_", newest.version)
            get$version.required[p] = newest.version
          }
          if(repo=="nexus"){
            stop("No <version> provided! Please provide a version (nexus error)!")
          }

        }
        cat("-------------", "\n")
        cat("Installing Requirement: [", p,"/",nrow(get), "]: ", get$name[p], "_", get$version.required[p], "\n", sep="")
        install_package_version(get$name[p], get$version.required[p],
                                lib.install.path=lib.install.path,
                                use.only.lib.install.path=use.only.lib.install.path,
                                cran.mirror = cran.mirror,
                                archiv.path = archiv.path,
                                main.path = main.path,
                                repo=repo,
                                auto.update.version.in.files= auto.update.version.in.files)
      }
    }

  }

  #detach package
  #capture.output(suppressWarnings(detach(paste0("package:",package), character.only = TRUE, force = TRUE)), file='NUL') # character.only = TRUE <- needed when paste0() or object used
  suppressWarnings(try(detach(paste0("package:",package), character.only = TRUE, force = T), silent = T))




  #Rekursive function
  install_and_check = function() {

    #Load all required packages/Namespaces with specified version -> thereby no older versions are load when trying to install
    if(!all(is.na(depends.on$Packages)) & nrow(na.omit(depends.on$Packages))>0){
      namespaces = na.omit(m) #m columns: name, version.required, version.installed  #old:depends.on$Packages
      #update_packages_search_path()
      cat("Load all required packages/Namespaces:", "\n")
      for(i in 1:nrow(namespaces)){
        try.lv = try(library_version(package = namespaces[i,"name"], version = namespaces[i,"version.required"]), silent = T)
        if(!inherits(try.lv, "try-error")) {
          cat(paste0(namespaces[i,"name"],"_",namespaces[i,"version.required"], " loaded!", "\n"))
        } else{
          try.lv = try(library_version(package = namespaces[i,"name"], version = namespaces[i,"version.installed"]), silent = T)
          if(!inherits(try.lv, "try-error")) {
            cat(paste0(namespaces[i,"name"],"_",namespaces[i,"version.installed"], " loaded!", "\n"))
          } else {
            warning(paste0("Version ", namespaces[i,"version.required"], "(required) and ", namespaces[i,"version.installed"], "(installed) of package ", namespaces[i,"name"], "could not be loaded!"))
            }
          }
      }
    }

    #Create folder to install package in: <package.name>_<version>
    if(!dir.exists(package.install.path)) dir.create(package.install.path)
    #Check if constructed url is correct
    cat("---------------------", "\n")
    check = suppressWarnings(try(readLines(package.url), silent = T)) # open.connection(url(),open="rt",timeout=t)
    #suppressWarnings(try(close.connection(url(package.url)),silent=T))


    #Install package if url (archive) is correct, use it
    if(!inherits(check, "try-error")) {
      cat("Installing package '", package, "' (version ", version.installing, ") from '", package.url, "' (and dependencies!).", "\n", sep="")
      #update_packages_search_path(path=lib.install.path)
      update_packages_search_path(install=TRUE) #keep only newest package versions in Namespace -> else old version of dependencies can deter installation of packages
      #update_packages_search_path(path=lib.install.path)
      utils::install.packages(package.url, repos=NULL, type="source", lib=package.install.path, INSTALL_opts="--no-test-load")
    } else{
      #try main page
        cat("(T0)")
        #new.package.url = paste0(cran.mirror, "src/contrib/", package, "_", version.installing, ".tar.gz") # don't look into "/Archive/" -> get newest version
        check = suppressWarnings(try(readLines(new.package.url),silent = T)) # open.connection(url(),open="rt",timeout=t
        #suppressWarnings(try(close.connection(url(new.package.url)),silent=T))
        if(!inherits(check, "try-error")) {
          cat("(T1) Installing package '", package, "' (version ", version.installing, ") from '", new.package.url, "' (and dependencies!).", "\n", sep="")
            #update_packages_search_path(path=lib.install.path)
            update_packages_search_path(install=TRUE) #keep only newest package versions in Namespace -> else old version of dependencies can deter installation of packages
            #update_packages_search_path(path=lib.install.path)
          utils::install.packages(new.package.url, repos=NULL, type="source", lib=package.install.path, INSTALL_opts="--no-test-load")
        } else {
          #try to change version structure e.g. from 0.1.10 to 0.1-1
          #e.g. dplyr_0.8.0 (https://cloud.r-project.org/src/contrib/Archive/dplyr/dplyr_0.8.0.tar.gz) depends on plogr 0.1-1
          #but there is only an version plogr 0.1.10 (https://cloud.r-project.org/src/contrib/Archive/plogr/, https://cloud.r-project.org/src/contrib/)
          version.installing = sub("0([^0]*)$", "\\1",sub(".([^.]*)$", "-\\1", version.installing)) #change 0.1.10 to 0.1-1
          #Replaces by build_package_url(): #new.package.url = paste0(cran.mirror, "src/contrib/Archive/", package, "/", package, "_", version.installing, ".tar.gz")
          new.package.url = build_package_url(.main=FALSE, .package=package, .version=version.installing)
          check = suppressWarnings(try(readLines(new.package.url),silent = T)) # open.connection(url(),open="rt",timeout=t
          #suppressWarnings(try(close.connection(url(new.package.url)),silent=T))
          if(!inherits(check, "try-error")) {
            cat("(T2) Installing package '", package, "' (version ", version.installing, ") from '", new.package.url, "' (and dependencies!).", "\n", sep="")
              #update_packages_search_path(path=lib.install.path)
              update_packages_search_path(install=TRUE) #keep only newest package versions in Namespace -> else old version of dependencies can deter installation of packages
              #update_packages_search_path(path=lib.install.path)
            utils::install.packages(new.package.url, repos=NULL, type="source", lib=package.install.path, INSTALL_opts="--no-test-load")
          } else {
              cat("(T3) Error!!! Package ", package, " (version: ",version.required, ") was not found in: \n", sep="")
              cat("- ", package.url, "\n", sep="")
              cat("- ", new.package.url, "\n", sep="")
          }
      }
    }


  #Load packages once in order to check if desired version can be used
  cat("Try to load packages from: ", package.install.path, "\n", sep ="")

  #Recursive unloading until all packages loading the package of interest
    recursive_unload = function(package) {
      unloaded = FALSE
      while(unloaded == F) {
        unloadmessage = try(unloadNamespace(p), silent = TRUE)
        if(is.null(unloadmessage)){
          cat("unloadNamespace: ", p, "\n")
          unloaded = TRUE
          return(TRUE)
        } else {
            preventing.unloading = regmatches(unloadmessage, gregexpr("ist importiert von (.*?) und kann deshalb nicht entladen werden", unloadmessage, perl = TRUE))[[1]]
            preventing.unloading = try(regmatches(preventing.unloading, gregexpr("(?<=‘|')\\S+(?=’|')", preventing.unloading, perl = TRUE))[[1]], silent=T)
            unloaded = recursive_unload(preventing.unloading)
        }
      }
    }


    #Unload all namespaces the packages of interest is imported in, in order to load it
      #e.g. ggmap_3.0.2 imports ggplot2, therefore loading a new ggplot2 version could cause an error, because the old one can't be unloaded
      exit = FALSE
      while(exit==FALSE) {
        message = try(library(package, lib.loc = package.install.path, character.only = TRUE), silent = TRUE)
        preventing.detaching = regmatches(message, gregexpr("ist importiert von (.*?) und kann deshalb nicht entladen werden", message, perl = TRUE))[[1]]
        preventing.detaching = try(regmatches(preventing.detaching, gregexpr("(?<=‘|')\\S+(?=’|')", preventing.detaching, perl = TRUE))[[1]], silent=T)
        if(!inherits(preventing.detaching, "try-error")) {
          for(p in preventing.detaching){
            recursive_unload(p)
          }
        } else exit = TRUE
      }


    #Check:
      error = try(library(package, lib.loc = package.install.path, character.only = TRUE), silent = TRUE) # character.only = TRUE <- needed when paste0() or object used
      if(!inherits(error, "try-error")){

        #update version of package in files if structure of version name on CRAN differs to required version
        if(auto.update.version.in.files==TRUE) {
          # #update folder name shell("rename <old> >new>) <- NOT NEEDED, already the correct name
            # shell(paste('rename',  paste0(lib.install.path,"/", package, "_", .version), paste0(lib.install.path,"/", package, "_", version))

          #update package.rds <- from this, r extracts the version of a package
            version.installing = version.required
            file = readRDS(file.path(package.install.path, package, "Meta/package.rds"))
            file[["DESCRIPTION"]][["Version"]] <- version.required # inserting the correct version name structure
            saveRDS(file, file.path(package.install.path, package, "Meta/package.rds"))
        }

        if(version.installing == utils::packageVersion(package)) cat(paste0("Check: Desired version (-> ", version.installing, ") of the package '", package, "' loaded! :)", "\n"))
        if(version.installing != utils::packageVersion(package)) cat(paste0("Check: Error!!! Version '", utils::packageVersion(package), "' instead of desired version ", version.installing, " of packages '", package, "' loaded! -.-"))
      return(TRUE)
      } else {
        cat(error[1])
        cat("---")
        cat("Installation of package ", package, " (version:", version.required,") was NOT successful! :(", "\n", sep ="")
        cat("Retry the installation one more time...", "\n")
        unlink(package.install.path, recursive = TRUE) # delete empty folder
        return(FALSE)
        }
    } # end of install_and_check()



  success = FALSE
  i = 1
  while(all(success == FALSE, i < 3)){ # Three attempts to install an package
    cat("\n", "----", "\n")
    cat(i, ". Attempt to install the package ", package, " (version:", version.required, ")\n", sep="")
    #detach before installing
    suppressWarnings(try(detach(paste0("package:", package), character.only = TRUE, force = T), silent = T))
    #try to install the package
    success = install_and_check()
    i =i+1
  }
  if(success==FALSE) {

    #try installation one more time, with another version
      #Select new version
        .new = find_package_version_on_cran(package, "999999") # user selects other version
        new.package.url = .new[1]
        rversion.required = .new[2]

      #Create folder to install the package in
        #cat("lib.install.path:", lib.install.path, "\n")
        package.install.path = paste0(lib.install.path,"/", package, "_", rversion.required)
        cat("Installing package '", package, "_", rversion.required, "' into folder: ", package.install.path, "...", "\n", sep="")
        if(!dir.exists(package.install.path)) dir.create(package.install.path)

      #Try installation
        utils::install.packages(new.package.url, repos=NULL, type="source", lib=package.install.path, INSTALL_opts="--no-test-load")
        #success = install_package_version(get$name[p], get$version.required[p],
        #                          lib.install.path=lib.install.path,
        #                          auto.update.version.in.files= auto.update.version.in.files)

      #Check if it worked
        error = try(library(package, lib.loc = package.install.path, character.only = TRUE), silent = TRUE) # character.only = TRUE <- needed when paste0() or object used
        if(!inherits(error, "try-error")){
          success=TRUE
        } else {
          success=FALSE
          unlink(package.install.path, recursive = TRUE) # delete empty folder
        }
  }


  if(success==FALSE) {
    cat("\n")
    cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
    cat("Your r-version and the specified packages version are not compatible! \n")
    cat("Installed r-version: ", rversion.installed, "\n")
    cat("By package (min.) required r-version: ", rversion.required, "\n")
    cat("-> Check whether you can use a package-version which depends on an r-version closer to yours \n")
    cat("-> or you can install an r-version which is closer to the required r-version of this package version. \n")
    cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
    cat("\n")
    return(FALSE)
  }

  if(success){
    #Detach the installed package again <- this mimics the behavior of install.packages()
      suppressWarnings(try(detach(paste0("package:",package), character.only = TRUE, force = T), silent = T))

    #Update the "Search Paths for Packages" (-> '.libPaths()')
      update_packages_search_path(path = package.install.path)  # c(lib.install.path, package.install.path)

    return(TRUE)
  }

}
