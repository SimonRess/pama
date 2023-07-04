#' Loading/Attaching specific versions of packages
#' @section Dependencies:
#' - update_packages_search_path()
#' - unloadRecursive()
#'
#' @rdname library_version
#'
#' @param package (chr value): Name of the Package
#' @param version (chr value): Version of the Package
#' @param lib.search.path (chr vector): In folder(s) which to search for the packages. NULL: use standard paths from `.libPaths()`
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


  #Check format of args
    if(!is.character(package)) {stop(paste0("'package = ", package, "' is not of type <character>. Please provide a character value!"))}
    if(!length(package)==1) {stop(paste0("'package = ", package, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(version)) {stop(paste0("'version = ", version, "' is not of type <character>. Please provide a character value!"))}
    if(!length(version)==1) {stop(paste0("'version = ", version, "' is not a single character value. Please provide a single character value!"))}

    if(!is.null(lib.search.path)) {
      if(!is.character(lib.search.path)) {stop(paste0("'lib.search.path = ", lib.search.path, "' is not of type <character>. Please provide a character vector!"))}
    }



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

    cat("Try to load packages from: ", lib.search.path, "\n", sep ="")



########################################################
# WORKS BUT TAKES TO MUCH TIME...
########################################################
    # #Initially load all dependencies
    #   #get packages dependencies
    #   .dep = get_dependencies(package, version, check.in.lib = TRUE)
    #   .dep = na.omit(.dep$Packages) # keep only dependencies requiring a specific version
    #
    #   #only only installed packages versions -> e.g. is 0.1.0 is required but only 0.2.0 is installed, use the latter instead
    #   .installed = get_installed_packages()
    #   .installed$installed = TRUE
    #   exact = merge(.dep, .installed, by = c("name", "version"), all.x = TRUE)
    #   highest = exact[is.na(exact$installed),]
    #   highest = .installed[.installed$name %in% highest$name,]
    #   highest = highest[with(highest, version == ave(version, name, FUN=max)),]
    #   exact = na.omit(exact)
    #
    #   #exact available + highest available
    #   .dep = rbind(exact, highest)
    #
    #   #load all dependencies = package versions
    #   if(nrow(.dep)>0){
    #     for(.p in 1: nrow(.dep)){
    #
    #       #.lib = .libPaths()[grepl(paste0(.dep[.p,"name"],"_",.dep[.p,"version"]), .libPaths())]
    #
    #       #Load package version if it not already loaded
    #         .currently_loaded = as.vector(apply(sapply(sessionInfo()$otherPkgs, \(x) x[c("Package", "Version")]), 2, \(x) paste(x, collapse = "_")))
    #         if(!(paste0(.dep[.p,"name"],"_",.dep[.p,"version"]) %in%.currently_loaded)){
    #           library_version(.dep[.p,"name"], .dep[.p,"version"]) # load package only if "name" & "version" is not missing
    #         }
    #     }
    #   }
########################################################


    #Unload all namespaces the packages of interest is imported in, in order to load it
      #e.g. ggmap_3.0.2 imports ggplot2, therefore loading a new ggplot2 version could cause an error, because the old one can't be unloaded


      #Loop until all version of packages unloaded who prevent to load the required version
        conflicting_versions = TRUE
        message = try(library(package, lib.loc = lib.search.path, character.only = TRUE), silent = TRUE)
        #if loading is not possible unload recursive all conflicting namespaces
          # -> if its a namespace-conflict -> "ist bereits geladen, aber" | "ist importiert von"
        if(!is.null(message) & (any(grepl("ist bereits geladen, aber",message)) | any(grepl("ist importiert von",message)))){
          while(conflicting_versions){
            conflicting_versions = unloadRecursive(lib_error_message = message) # unload conflicting versions\namespaces, track if there are still conflicting_versions
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
