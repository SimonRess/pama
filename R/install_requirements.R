#' ...
#' @section Dependencies:
#' - get_requirements()
#'
#' - get_installed_packages()
#'
#' - install_package_version()
#'
#' @rdname install_requirements
#'
#' @param req.file.path (chr vector): Name of the requirements-file
#' @param req.file.name (chr vector): Folder of the requirements-file
#' @param lists (chr vector): Names of lists (without '#' !) specified in the requirements-file to use
#' @param library.folder.path (chr vector): Folder in which the subfolder "lib" should be created
#' @param library.folder.name (chr vector): Name of the lib-folder
#'
#' @details Wrapper around get_requirements() and install_package_version(). Installs all packages from the requirements-file (+ dependencies) in a lib-folder.
#'
#' @section Side effects: Installation of the package & adding package location to the search paths
#' @section Return: None
#' @export
#'
#' @keywords requirements-file installing package-versions
#' @seealso \code{\link[base]{.libPaths}}
#'
#' @examples
#' \dontrun{
#' # use 'requirements.txt.' within the current working directory
#' install_requirements()
#'
#' # use 'req.txt' within the lib folder of the current working directory
#' install_requirements(req.file.path=paste(getwd(),"lib",sep="/"),
#'                      req.file.name="requirements.txt",
#'                      lists="all",
#'                      library.folder.path=getwd(),
#'                      library.folder.name="lib")
#' }
#'
#' @author Simon Ress


install_requirements = function(req.file.path=getwd(), req.file.name="requirements.txt", lists="all",  library.folder.path=getwd(), library.folder.name="lib") {
  # Reads the requirements-file and outputs a list of packages to install/load
  # :param req.file.name (chr vector): Name of the requirements-file (-> USE .txt-file !!!)
  # :param req.file.path (chr vector): Path to the requirements-file
  # :param list (chr vector): Name(s) of the lists to install from the requirements-file
  # :param library.folder.name (chr vector): name of the folder packages will be installed in (create folder if not existing)
  # :param library.folder.path (chr vector): path the folder packages will be installed in (create folder if not existing)
  # :return:            ???             List of list including vectors with packages to install/load: <package_name> <version>
  # :side-effects: none


  #library.folder.path = getwd()
  #library.folder.name = "lib"

  #C:\Users\simon\AppData\Local\R\win-library\4.2
  #D:\OneDrive - MT AG\Projects\22.07-_ - AOK\CRAN_package.version\Project


  req = get_requirements(file.name=req.file.name, path=req.file.path)


  #check R version
  rversion.installed = paste0(R.version$major,".",R.version$minor)
  rversion.required = req[["r.version"]]

  if(rversion.installed>=rversion.required) cat("Installed R-Version is sufficient (installed:",rversion.installed," >= required:",rversion.required,")", "\n", sep="")
  if(rversion.installed<rversion.required) {
    cat("Installed R-Version is NOT sufficient (installed:",rversion.installed," < required:",rversion.required,")\n", sep="")
    install = ""
    while(toupper(install)!="Y" & toupper(install)!="N") {
      install <- readline(prompt=paste0("Do you want to install the required R-Version (",rversion.required,") now [Y/N]?: "))
      if(toupper(install) == "Y") cat("Not implementet yet (dependency 'installr' & 'devtools' would be needed, what is not desired). \n -> Please install R-version ",rversion.required," by your own and try install_requirements() again. \n", sep="")
      if(toupper(install) == "N") cat("-> Please install R-version ",rversion.required," by your own and try install_requirements() again. \n", sep="")
    }
  }

  if(rversion.installed!=rversion.required) {
    cat("The installed r-version does not match the required r-version (installed:",rversion.installed," < required:",rversion.required,")\n", sep="")
    install = ""
    while(toupper(install)!="Y" & toupper(install)!="N") {
      install <- readline(prompt=paste0("Do you want to install the required R-Version (",rversion.required,") now [Y/N]?: "))
      if(toupper(install) == "Y") cat("Not implementet yet (dependency 'installr' & 'devtools' would be needed, what is not desired). \n -> Please install R-version ",rversion.required," by your own and try install_requirements() again. \n", sep="")
      if(toupper(install) == "N") cat("-> Please install R-version ",rversion.required," by your own and try install_requirements() again. \n", sep="")
    }
  }


  #  #Check already installed packages & R-verison
  #    already.installed = get_installed_packages()
  #    #keep only the highest installed version
  #    already.installed = merge(aggregate(version ~ name, max, data = already.installed), already.installed)
  #
  #  #Uninstalled but required packages or installed version too old
  #    m = merge(depends.on$Packages, already.installed, by="name", all.x = TRUE, all.y = FALSE, suffixes = c(".required",".installed"))
  #    #print(m)
  #    get1 = m[is.na(m$version.installed),] # required, but not installed (or not version available)
  #    #print(get1)
  #    get2 = m[!is.na(m$version.required) & m$version.required > m$version.installed,] # required version > installed version
  #    #print(get2)
  #    get = rbind(get1,get2)
  #    if(nrow(get)>0){
  #      print("Unsatisfied requirements: \n")
  #      print(get)
  #    }else print("All requirements satisfied: \n")



  #install specific versions of packages
  #Install all packages
  if(lists == "all"){

    req.packages = as.vector(unique(unlist(req[-1]))) # keep only packages / remove duplicates
    req.packages = req.packages[!is.na(req.packages)] # delete NAs

    lib = paste0(library.folder.path, "/", library.folder.name)

    if(!dir.exists(lib)) dir.create(lib)

    update_packages_search_path()

    for(p in req.packages) {
      cat("-----------------------------------------------------", "\n")
      cat("Install: ", p, "\n")
      #detach all (none base) packages -> required because detaches are necessary in the process, but dependencies may prevent some
      utils::capture.output(suppressWarnings(lapply(paste('package:',names(utils::sessionInfo()$otherPkgs),sep=""), \(x) try(detach(x, character.only=TRUE,unload=TRUE,force = TRUE),silent = T))), file='NUL')
      package = strsplit(p, " ")[[1]][1]
      version = strsplit(p, " ")[[1]][2]
      install_package_version(package, version, lib.install.path=lib)
    }

    #Install main + selected lists
  }else{
    #list=c("#vizualisation","#statistics")

    #Check whether all lists are available, if so:
    if(all(lists %in% names(req[-1]))){
      #select packages from entered lists + "main"-list
      req.packages = as.vector(unique(unlist(req[c("main", lists)])))

      lib = paste0(library.folder.path, "/", library.folder.name)

      if(!dir.exists(lib)) dir.create(lib)

      update_packages_search_path()

      for(p in req.packages) {
        cat("-----------------------------------------------------", "\n")
        cat("Install: ", p, "\n")
        #detach all (none base) packages -> required because detaches are necessary in the process, but dependencies may prevent some
        utils::capture.output(suppressWarnings(lapply(paste('package:',names(utils::sessionInfo()$otherPkgs),sep=""), \(x) try(detach(x, character.only=TRUE,unload=TRUE,force = TRUE),silent = T))), file='NUL')
        package = strsplit(p, " ")[[1]][1]
        version = strsplit(p, " ")[[1]][2]
        install_package_version(package, version, lib.install.path=lib)
      }
      #if not:
    }else{
      cat("Error: Some entered lists are not available in '", paste(req.file.path,req.file.name,sep="/"), "'!", "\n", sep="")
      cat("- Lists in reqirement-file:", paste0("'", names(req[-1]),"'", collapse = ", "), "\n")
      cat("- Entered but not available list(s):", paste0("'",lists[!(lists %in% names(req[-1]))],"'", collapse = ", "), "\n")
    }

  }

}
