#' Loading/Attaching specific versions of packages
#' @section Dependencies: - update_packages_search_path()
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

library_version = function(package, version, lib.search.path = .libPaths()[1]){
  # Loads a specific version of a package. The package folder must be with a folder called <package-name>_<version> which itself is within .libPaths()[1]
  # :param package (string): Optional location for the downloaded files,
  # :param version (string): Year of the data
  # :return: none
  # :side-effects: load the namespace of the package with name <package> & version <version> and attach it on the search list
  if(!is.character(package)) {warning("Provide the package name as string (e.g. 'ggplot2')"); stop()}
  if(!is.character(version)) {warning("Provide the version name as string (e.g. '3.1.0')"); stop()}

  #detach other already loaded versions of the package
  suppressWarnings(try(detach(paste0("package:",package), character.only = TRUE, force = T), silent = T))

  #Load specified version of the package

    #Unload all namespaces the packages of interest is imported in, in order to load it
      #e.g. ggmap_3.0.2 imports ggplot2, therefore loading a new ggplot2 version could cause an error, because the old one can't be unloaded

    cat("Try to load packages from: ", package.install.path, "\n", sep ="")
    exit = FALSE
    while(exit==FALSE) {
      message = try(library(package, lib.loc = package.install.path, character.only = TRUE), silent = TRUE)
      preventing.detaching = regmatches(message, gregexpr("ist importiert von (.*?) und kann deshalb nicht entladen werden", message, perl = TRUE))[[1]]
      preventing.detaching = try(regmatches(preventing.detaching, gregexpr("(?<=‘|')\\S+(?=’|')", preventing.detaching, perl = TRUE))[[1]], silent=T)
      if(!inherits(preventing.detaching, "try-error")) {
        for(p in preventing.detaching){
          cat("unloadNamespace: ", p, "\n")
          unloadNamespace(p)
          #.libPaths(.libPaths()[-grep(p,.libPaths())])
          #suppressWarnings(try(detach(paste0("package:",p), character.only = TRUE, force = T), silent = T))
        }
      } else exit = TRUE
    }

    #test if loading worked
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
