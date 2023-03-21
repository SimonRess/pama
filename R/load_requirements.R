#' Loading/Attaching all packages by version specified in a requirements-file
#' @section Dependencies:
#' - update_packages_search_path()
#'
#' - get_requirements()
#'
#' - library_version()
#'
#' @rdname load_requirements
#'
#' @param req.file.path (chr vector): Name of the requirements-file
#' @param req.file.name (chr vector): Folder of the requirements-file
#' @param lists (chr vector): Names of lists (without '#' !) specified in the requirements-file to use
#' @param library.folder.path (chr vector): Folder in which the subfolder "lib" should be created
#' @param library.folder.name (chr vector): Name of the lib-folder
#'
#' @details Wrapper around get_requirements() and library_version(). Loading/Attatching all packages from the requirements-file.
#'
#' @section Side effects: Loading/Attatching all packages from the requirements-file.
#' @section Return: None
#' @export
#'
#' @keywords requirements-file loading package-versions library
#' @seealso \code{\link[utils]{.libPaths()}}
#'
#' @examples
#' \dontrun{
#' # load all packages specified in the 'requirements.txt.' within the current working directory
#' detach_none_base() # detach all none base packages
#' load_requirements()
#' as.vector(apply(sapply(sessionInfo()$otherPkgs, \(x) x[c("Package", "Version")]), 2, \(x) paste(x, collapse = "_"))) # currently loaded packages incl. version
#'
#'
#' # load packages specified as main packages AND packages specified within the list '#statistics' in the 'requirements.txt.' within the current working directory
#' detach_none_base() # detach all none base packages
#' load_requirements(lists = "#statistics")
#' as.vector(apply(sapply(sessionInfo()$otherPkgs, \(x) x[c("Package", "Version")]), 2, \(x) paste(x, collapse = "_"))) # currently loaded packages incl. version
#'
#'
#' # load packages specified as main packages AND packages specified within the lists '#statistics' & '#data_wrangling' in the 'requirements.txt.' within the current working directory
#' detach_none_base() # detach all none base packages
#' load_requirements(lists = c("#statistics", "#data_wrangling"))
#' as.vector(apply(sapply(sessionInfo()$otherPkgs, \(x) x[c("Package", "Version")]), 2, \(x) paste(x, collapse = "_"))) # currently loaded packages incl. version

#' }
#'
#' @author Simon Ress


load_requirements = function(req.file.path=getwd(), req.file.name="requirements.txt", lists="all",  library.folder.path=getwd(), library.folder.name="lib") {
  # Reads the requirements-file and outputs a list of packages to install/load
  # :param req.file.name (chr vector): Path to the requirements-file
  # :param req.file.path (chr vector): Name of the requirements-file (-> USE .txt-file !!!)
  # :param list (chr vector): Name(s) of the lists to load from the requirements-file
  # :param library.folder.path (chr vector): Path to the folder (e.g. "lib") where packages from the requirements-file are installed in
  # :param library.folder.name (chr vector): Name of the folder (e.g. "lib") where packages from the requirements-file are installed in
  # :return: ???
  # :side-effects: Loading all selected package-lists from the requirements-file

  #detaching all other (non base) packages, before loading packages from requirements-file
    detach_none_base()


  #get content of requirements-file
  req = get_requirements(file.name=req.file.name, path=req.file.path)

  #update package search paths
  lib = paste(library.folder.path, library.folder.name, sep="/")
  update_packages_search_path(lib)
  update_packages_search_path()

  #load specific versions of packages
  #load all packages
  if(lists == "all"){

    req.packages = as.vector(unique(unlist(req[-1]))) # keep only packages / remove duplicates
    req.packages = req.packages[!is.na(req.packages)] # delete NAs

    rq = req.packages

    #loop until all packages are loaded
      # necessary because some packaged need to unload others when loaded, therefore order of the packages would be important without the while-loop
      # e.g. ggplot2 is imported by ggmap. In order to load ggplot2 we need to detach ggmap. But when ggmap is named before ggplot2 in requirements-file
      # ggmap will be loaded and then unloaded in order to load the specified ggplot2-version -> result without loop: only ggplot2 would be loaded
    while(length(rq)>0) {

      for(p in rq) {
        cat("-----------------------------------------------------", "\n")
        cat("Load: ", p, "\n")
        #update_packages_search_path(lib)
        #detach all (none base) packages -> required because detaches are necessary in the process, but dependencies may prevent some
        #capture.output(suppressWarnings(lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""), \(x) try(detach(x, character.only=TRUE,unload=TRUE,force = TRUE),silent = T))), file='NUL')
        package = strsplit(p, " ")[[1]][1]
        version = strsplit(p, " ")[[1]][2]
        # find folder with package-version inside
        lib = .libPaths()[grep(paste0(package, "_", version), .libPaths())]
          #when is package-folder not found
          if(identical(lib,character(0))) stop(paste0("Package ", package, " (version: ", version, ") not found. Install it first!"))
        library_version(package, version, lib.search.path=lib)
      }

    rq = req.packages[!(sub(" ", "_", req.packages) %in% as.vector(apply(sapply(utils::sessionInfo()$otherPkgs, \(x) x[c("Package", "Version")]), 2, \(x) paste(x, collapse = "_"))))]
    }


    return(as.vector(apply(sapply(utils::sessionInfo()$otherPkgs, \(x) x[c("Package", "Version")]), 2, \(x) paste(x, collapse = "_"))))

  #load main + selected lists
  }else{
    #lists=c("#vizualisation","#statistics")

    #Check whether all lists are available, if so:
    if(all(lists %in% names(req[-1]))){
      #select packages from entered lists + "main"-list
      req.packages = as.vector(unique(unlist(req[c("main", lists)])))

      for(p in req.packages) {
        cat("-----------------------------------------------------", "\n")
        cat("Load: ", p, "\n")
        #detach all (none base) packages -> required because detaches are necessary in the process, but dependencies may prevent some
        #capture.output(suppressWarnings(lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""), \(x) try(detach(x, character.only=TRUE,unload=TRUE,force = TRUE),silent = T))), file='NUL')
        package = strsplit(p, " ")[[1]][1]
        version = strsplit(p, " ")[[1]][2]
        # find folder with package-version inside
        lib = .libPaths()[grep(paste0(package, "_", version), .libPaths())] # find folder with package-version inside
          #when is package-folder not found
          if(identical(lib,character(0))) stop(paste0("Package ", package, " (version: ", version, ") not found. Install it first!"))
        library_version(package, version, lib.search.path=lib)

      }
      return(as.vector(apply(sapply(utils::sessionInfo()$otherPkgs, \(x) x[c("Package", "Version")]), 2, \(x) paste(x, collapse = "_"))))
      #if not:
    }else{
      cat("Error: Some entered lists are not available in '", paste(req.file.path,req.file.name,sep="/"), "'!", "\n", sep="")
      cat("- Lists in reqirement-file:", paste0("'", names(req[-1]),"'", collapse = ", "), "\n")
      cat("- Entered but not available list(s):", paste0("'",lists[!(lists %in% names(req[-1]))],"'", collapse = ", "), "\n")
    }
  }


}
