#' Loading/Attaching specific versions of packages
#' @section Dependencies:
#' - update_packages_search_path()
#' - unloadRecursive()
#'
#' @rdname library_version
#'
#' @param package (chr vector): Name of the Package
#' @param version (chr vector): Version of the Package
#' @param lib.search.path (chr vector): Folder in which to install the packages
#'
#' @details Loads a specific version of a package. The package folder must be with a folder called <package-name>_<version> which itself is within .libPaths()[1]
#'
#' @section Side effects: Loading/Attaching of the package version
#' @section Return: None
#' @export
#'
#' @keywords Updates Search Paths for Packages
#' @seealso \code{\link[base]{.libPaths}}
#'
#' @examples
#' \dontrun{
#' # Load v3.4.0 of ggplot2 from folder "ggplot2_3.4.0" in search paths
#' library_version("ggplot2", "3.4.0")
#' }
#'
#' @author Simon Ress

library_version = function(package, version, lib.search.path = NULL){
  # Loads a specific version of a package. The package folder must be with a folder called <package-name>_<version> which itself is within .libPaths()[1]
  # :param package (string): Optional location for the downloaded files,
  # :param version (string): Year of the data
  # :return: none
  # :side-effects: load the namespace of the package with name <package> & version <version> and attach it on the search list
  if(!is.character(package)) {warning("Provide the package name as string (e.g. 'ggplot2')"); stop()}
  if(!is.character(version)) {warning("Provide the version name as string (e.g. '3.1.0')"); stop()}

  #If no path is specified: 1. update_packages_search_path 2.use all search paths
  if(is.null(lib.search.path)) {
    update_packages_search_path()
    lib.search.path = .libPaths()
  }


  # find folder with package-version inside
  lib.search.path = lib.search.path[grep(paste0(package, "_", version), lib.search.path)] # find folder with package-version inside
    #when is package-folder not found
    if(identical(lib.search.path,character(0))) stop(paste0("Package ", package, " (version: ", version, ") not found. Install it first!"))

  #detach other already loaded versions of the package
  suppressWarnings(try(detach(paste0("package:",package), character.only = TRUE, force = T), silent = T))

  #Load specified version of the package

    #Unload all namespaces the packages of interest is imported in, in order to load it
      #e.g. ggmap_3.0.2 imports ggplot2, therefore loading a new ggplot2 version could cause an error, because the old one can't be unloaded

    cat("Try to load packages from: ", lib.search.path, "\n", sep ="")
    # exit = FALSE
    # while(exit==FALSE) {
    #   message = try(library(package, lib.loc = lib.search.path, character.only = TRUE), silent = TRUE)
    #
    #   #If there's an loading-error connected to depending packages preventing unloading the package -> unload these other packages first
    #   if(inherits(message, "try-error") & any(grepl("ist importiert von (.*?) und kann deshalb nicht entladen werden", message))) {
    #     #Get packages that prevent unloaded
    #     preventing.detaching = regmatches(message, gregexpr("ist importiert von (.*?) und kann deshalb nicht entladen werden", message, perl = TRUE))[[1]]
    #     preventing.detaching = try(regmatches(preventing.detaching, gregexpr("(?<=‘|')\\S+(?=’|')", preventing.detaching, perl = TRUE))[[1]], silent=T)
    #
    #     for(p in preventing.detaching){
    #       cat("unloadNamespace: ", p, "\n")
    #       unloadNamespace(p)
    #       #.libPaths(.libPaths()[-grep(p,.libPaths())])
    #       #suppressWarnings(try(detach(paste0("package:",p), character.only = TRUE, force = T), silent = T))
    #     }
    #   } else exit = TRUE
    # }


    # test if loading worked

        #   #Loop until all version of packages unloaded who prevent to load the required version
        #   conflicting_versions = TRUE
        #   while(conflicting_versions){
        #     message = try(library(package, lib.loc = lib.search.path, character.only = TRUE), silent = TRUE)
        #     conf = regmatches(message, gregexpr("Namensraum (.*?) ist bereits geladen, aber", message, perl = TRUE))[[1]]
        #     if(!identical(character(0), conf)){
        #       conf = try(regmatches(conf, gregexpr("(?<=‘)\\s*(.*?)\\s+(?=ist)", conf, perl = TRUE))[[1]], silent=T)
        #       conf_p = trimws(strsplit(conf, "’ ")[[1]][1])
        #       conf_namespaces = try(unloadNamespace(conf_p), silent = TRUE)
        #       if(!is.null(conf_namespaces)){
        #         conf_namespaces = regmatches(conf_namespaces, gregexpr("(?<=importiert von)\\s*(.*?)\\s+(?=und kann)", conf_namespaces, perl = TRUE))[[1]]
        #         for(conf_namespace in strsplit(gsub("‘|’","", conf_namespaces), ",")[[1]]){
        #           conf_namespaces = try(unloadNamespace(trimws(conf_namespace)), silent = TRUE)
        #         }
        #
        #       }
        #
        #     }
        #
        #   }


    #Loop until all version of packages unloaded who prevent to load the required version
      conflicting_versions = TRUE
      message = try(library(package, lib.loc = lib.search.path, character.only = TRUE), silent = TRUE)
      #if loading is not possible unload recursive all conflicting namespaces
        # -> if its a namespace-conflict -> "ist bereits geladen, aber" | "ist importiert von"
      if(!is.null(message) & (any(grepl("ist bereits geladen, aber",message)) | any(grepl("ist importiert von",message)))){
        while(conflicting_versions){
          conflicting_versions = unloadRecursive(lib_error_message = message)
          message = try(library(package, lib.loc = lib.search.path, character.only = TRUE), silent = TRUE)
          try(attachNamespace(loadNamespace(package, lib.loc = lib.search.path)), silent = TRUE) #ensure that the namespace is attached
          # message = library(rlang, lib.loc = "C:/Users/sress/Desktop/titanic-r-master/lib/rlang_1.0.6")
        }
      }



    #if not:
    if(inherits(message, "try-error")) {
      cat("ERROR MESSAGE: ", message[1])
      cat("----")
      stop(paste0("The requested version of the packages can't be loaded!
        -> 1. Check the entered package name and version number (e.g. package= 'ggplot2' & version = '3.0.0')
        -> 2. Check if there is a folder named '",package, "_", version, "' in '", lib.search.path, "' which contains the package-folder '", package, "'."))
    } else {
      #if loading worked:
      cat("Package ", package, " (version: ", version, ") successfuly loaded.", "\n", sep="")
      #update search path
      #update_packages_search_path(package.install.path)
    }

    #test whether desired version is now loaded or not
    if(version == utils::packageVersion(package)) message(paste0("Check: Correct Version (->", version, ") of packages '", package, "' loaded"))
    if(version != utils::packageVersion(package)) warning(paste0("Check: Version '", utils::packageVersion(package), "' instead of desired version ", version, " of packages '", package, "' loaded"))
}
