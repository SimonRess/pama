#' Install packages by versions specified in the requirements-file
#' @section Dependencies:
#' - get_requirements()
#'
#' - get_installed_packages()
#'
#' - install_package_version()
#'
#' - detach_none_base()
#'
#' @rdname install_requirements
#'
#' @param cran.mirror (chr value): Main url of the cran mirror to use (e.g. "https://cloud.r-project.org/")
#' @param archiv.path (chr value): URL-path to the archive of the cran mirror to use (e.g. "src/contrib/Archive/")
#' @param main.path (chr value): URL-path to the pages main page of the cran mirror to use (e.g. "src/contrib/"")
#' @param repo (chr value): Which type of repo is used? This information is utilized when building the URL,
#' because each repo has its own file structure. Currently supported: "cran" & "nexus"
#'
#' @param req.file.path (chr value): Name of the requirements-file
#' @param req.file.name (chr value): Folder of the requirements-file
#' @param lists (chr vector): Names of lists (without '#' !) specified in the requirements-file to use
#' @param library.folder.path (chr value): Folder in which the subfolder "lib" should be created
#' @param library.folder.name (chr value): Name of the lib-folder
#' @param use.only.lib.install.path (bool): Only check in <lib.install.path> for installed packages and dependencies
#'
#' @param auto.update.version.in.files (bool):
#' If TRUE the version of a package in the installed files will be changed to the required version. This only happens if
#' it's the same version but the structure of the version name differs e.g. 0.1.10 to 0.1.-1 see \code{\link[pama]{find_package_version_on_cran}}
#' When FALSE nothing happens
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


install_requirements = function(cran.mirror = "https://cloud.r-project.org/",
                                archiv.path = "src/contrib/Archive/",
                                main.path = "src/contrib/",
                                repo="cran",
                                req.file.path=getwd(),
                                req.file.name="requirements.txt",
                                lists="all",
                                library.folder.path=getwd(),
                                library.folder.name="lib",
                                use.only.lib.install.path=TRUE,
                                auto.update.version.in.files = TRUE) {


  #args-Checks
    if(!is.character(cran.mirror)) {stop(paste0("'cran.mirror = ", cran.mirror, "' is not of type <character>. Please provide a character value!"))}
    if(!length(cran.mirror)==1) {stop(paste0("'cran.mirror = ", cran.mirror, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(archiv.path)) {stop(paste0("'archiv.path = ", archiv.path, "' is not of type <character>. Please provide a character value!"))}
    if(!length(archiv.path)==1) {stop(paste0("'archiv.path = ", archiv.path, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(main.path)) {stop(paste0("'main.path = ", main.path, "' is not of type <character>. Please provide a character value!"))}
    if(!length(main.path)==1) {stop(paste0("'main.path = ", main.path, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(repo)) {stop(paste0("repo' is not an character value. Provide an character value!"))}
    if(!length(repo)==1) {stop(paste0("repo' is not of length==1. Provide exactly one package name!"))}


    if(!is.character(req.file.path)) {stop(paste0("'req.file.path = ", req.file.path, "' is not of type <character>. Please provide a character value!"))}
    if(!length(req.file.path)==1) {stop(paste0("'req.file.path = ", req.file.path, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(req.file.name)) {stop(paste0("'req.file.name = ", req.file.name, "' is not of type <character>. Please provide a character value!"))}
    if(!length(req.file.name)==1) {stop(paste0("'req.file.name = ", req.file.name, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(lists)) {stop(paste0("'lists = ", lists, "' is not of type <character>. Please provide a character vector!"))}

    if(!is.character(library.folder.path)) {stop(paste0("'library.folder.path = ", library.folder.path, "' is not of type <character>. Please provide a character value!"))}
    if(!length(library.folder.path)==1) {stop(paste0("'library.folder.path = ", library.folder.path, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(library.folder.name)) {stop(paste0("'library.folder.name = ", library.folder.name, "' is not of type <character>. Please provide a character value!"))}
    if(!length(library.folder.name)==1) {stop(paste0("'library.folder.name = ", library.folder.name, "' is not a single character value. Please provide a single character value!"))}

    if(!is.logical(use.only.lib.install.path)) {stop(paste0("'use.only.lib.install.path = ", use.only.lib.install.path, "' is not of type <bool>. Please provide a boolean!"))}

    if(!is.logical(auto.update.version.in.files)) {stop(paste0("'auto.update.version.in.files = ", auto.update.version.in.files, "' is not of type <bool>. Please provide a boolean!"))}




  req = get_requirements(req.file.name=req.file.name, req.file.path=req.file.path)


  #check R version
  rversion.installed = paste0(R.version$major,".",R.version$minor)
  rversion.required = req[["r.version"]]

  if(rversion.installed>=rversion.required) cat("Installed R-Version is sufficient (installed:",rversion.installed," >= required:",rversion.required,")", "\n", sep="")
  if(rversion.installed<rversion.required) {
    cat("Installed R-Version is NOT sufficient (installed:",rversion.installed," < required:",rversion.required,")\n", sep="")
    install = ""
    # while(toupper(install)!="Y" & toupper(install)!="N") {
    #   install <- readline(prompt=paste0("Do you want to install the required R-Version (",rversion.required,") now [Y/N]?: "))
    #   if(toupper(install) == "Y") cat("Not implementet yet (dependency 'installr' & 'devtools' would be needed, what is not desired). \n -> Please install R-version ",rversion.required," by your own and try install_requirements() again. \n", sep="")
    #   if(toupper(install) == "N") cat("-> Please install R-version ",rversion.required," by your own and try install_requirements() again. \n", sep="")
    # }
  }

  if(rversion.installed!=rversion.required) {
    cat("The installed r-version does not match the required r-version (installed:",rversion.installed," != required:",rversion.required,")\n", sep="")
    install = ""
    # while(toupper(install)!="Y" & toupper(install)!="N") {
    #   install <- readline(prompt=paste0("Do you want to install the required R-Version (",rversion.required,") now [Y/N]?: "))
    #   if(toupper(install) == "Y") cat("Not implementet yet (dependency 'installr' & 'devtools' would be needed, what is not desired). \n -> Please install R-version ",rversion.required," by your own and try install_requirements() again. \n", sep="")
    #   if(toupper(install) == "N") cat("-> Please install R-version ",rversion.required," by your own and try install_requirements() again. \n", sep="")
    # }
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

    update_packages_search_path()

    #Remove already installed packages from vector
      for(pv in req.packages) {
        f = grep(sub(" ", "_", pv), .libPaths())
        if(!identical(f, integer(0))) {
          cat("Package '", pv, "' will not be installed!", "\n", sep="")
          cat("(Already installed in: ", .libPaths()[f], ")", "\n", sep="")
          req.packages = req.packages[req.packages!=pv]
        }
      }


    #create lib-folder to install the packages in
      lib = paste0(library.folder.path, "/", library.folder.name)
      if(!dir.exists(lib)) dir.create(lib)


    #install packages
      for(p in req.packages) {
        cat("-----------------------------------------------------", "\n")
        cat("Install: ", p, "\n")
        #detach all (none base) packages -> required because detaches are necessary in the process, but dependencies may prevent some
        detach_none_base()
        package = strsplit(p, "_")[[1]][1]
        version = strsplit(p, "_")[[1]][2]
        install_package_version(package, version,
                                lib.install.path=lib,
                                use.only.lib.install.path=use.only.lib.install.path,
                                cran.mirror=cran.mirror,
                                archiv.path=archiv.path,
                                main.path=main.path,
                                repo=repo,
                                auto.update.version.in.files=auto.update.version.in.files)
      }

  #Install main + selected lists
  } else{
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
        #utils::capture.output(suppressWarnings(lapply(paste('package:',names(utils::sessionInfo()$otherPkgs),sep=""), \(x) try(detach(x, character.only=TRUE,unload=TRUE,force = TRUE),silent = T))), file='NUL')
        detach_none_base()
        package = strsplit(p, "_")[[1]][1]
        version = strsplit(p, "_")[[1]][2]
        install_package_version(package, version,
                                lib.install.path=lib,
                                use.only.lib.install.path=use.only.lib.install.path,
                                cran.mirror=cran.mirror,
                                archiv.path=archiv.path,
                                main.path=main.path,
                                repo=repo,
                                auto.update.version.in.files=auto.update.version.in.files)
      }
      #if not:
    }else{
      cat("Error: Some entered lists are not available in '", paste(req.file.path,req.file.name,sep="/"), "'!", "\n", sep="")
      cat("- Lists in reqirement-file:", paste0("'", names(req[-1]),"'", collapse = ", "), "\n")
      cat("- Entered but not available list(s):", paste0("'",lists[!(lists %in% names(req[-1]))],"'", collapse = ", "), "\n")
    }

  }

}
