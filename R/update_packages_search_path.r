#' Updating Search Paths for Packages
#' @section Dependencies: None
#'
#' @rdname update_packages_search_path
#'
#' @param path (chr value | NULL): Location of R library tree which should be added to the search path. NULL: Just updating the search paths
#' @param print.infos (bool): Regulates output in the users console. TRUE: All internal information will be printed (useful for debugging). FALSE (by default): No or much less information will be printed.
#' @param install (bool): Set to TRUE if update_packages_search_path() is used within the installation of a package. Then it keep only the paths to the "newest" package version
#' @param install.path (chr value | NULL): If update_packages_search_path() is used within the installation of a package, add here the path where the package was installed. It will be added to the search paths. <NULL>: keep only the "newest" package version
#'
#' @details test
#'
#' @section Side effects: Updating Search Paths for Packages, see .libPaths()
#' @section Return: None
#' @export
#'
#' @keywords Updates Search Paths for Packages
#' @seealso \code{\link[base]{.libPaths}}
#'
#' @examples
#' \dontrun{
#' #Adding all version-specific packages folders within current search paths to the search paths
#' .libPaths()
#' update_packages_search_path()
#' .libPaths()
#'
#' #Adding a specific search path
#' .libPaths()
#' update.packages.search.path("C:/Users/Simon/AppData/Local/R/win-library/4.2/ggplot2_3.1.0")
#' .libPaths()
#' }
#'
#' @author Simon Ress

update_packages_search_path = function(path = NULL,
                                       install = FALSE,
                                       install.path = NULL,
                                       print.infos = FALSE) {
  # Dependencies: NONE
  #
  # Updates by default the "Search Paths for Packages" (-> '.libPaths()') by searching for folders with name <package_name>_<version> and adding these as search paths
  # Also a vector of paths with should be added to the search list can be specified
  # :path (chr vector): Paths be added to the search list, e.g "C:/Users/simon/AppData/Local/R/win-library/4.2/ggplot2_3.1.1" or c(<path1>, <path2>)
  # :return: None
  # :side-effects: Updating Search Paths for Packages, see .libPaths()


  #Check format of args
    if(!is.null(path)){
      if(!is.character(path)) {stop(paste0("'path = ", path, "' is not of type <character> or <NULL>. Please provide a character value or <NULL>!"))}
      if(!length(path)==1) {stop(paste0("'path = ", path, "' is not a single character value. Please provide a single character value or <NULL>!"))}
    }

    if(!is.logical(install)) {stop(paste0("'install = ", install, "' is not of type <bool>. Please provide a boolean!"))}

    if(!is.null(install.path)){
      if(!is.character(install.path)) {stop(paste0("'install.path = ", install.path, "' is not of type <character>. Please provide a character value!"))}
      if(!length(install.path)==1) {stop(paste0("'install.path = ", install.path, "' is not a single character value. Please provide a single character value!"))}
    }

    if(!is.logical(print.infos)) {stop(paste0("'print.infos = ", print.infos, "' is not of type <bool>. Please provide a boolean!"))}



  #Default updating of search paths by adding "package-version"-folders
    if(is.null(path)) {
      if(dir.exists(file.path(getwd(), "lib"))) {
        lib.paths = .libPaths() #Dont use always "lib"-folder, sometime you dont want it! # unique(c(normalizePath(.libPaths()), normalizePath(file.path(getwd(), "lib"))))
        } else lib.paths = .libPaths() # always ALSO search in "lib" in project-folder
      for(p in lib.paths) {
        package = dir(p)[which(grepl("_", dir(p)))] # greps folders which contains a "_" -> e.g "...\ggplot2_3.1.0"
        if(install==TRUE) package = package[!duplicated(sapply(package, \(x) strsplit(x,"_")[[1]][1]), fromLast = TRUE)] #keep only the "newest" package version
        if(length(package)>=1){
          paths.to.add = paste0(p, "\\", package)
          if(install==FALSE){
            #.libPaths(c(.libPaths(), paths.to.add)) # .libPaths(new) replaces always to first element, in order to keep it use the construct: .libPaths(c(.libPaths(), new))
            modify_libPaths(add=paths.to.add)
            for(i in paths.to.add) {
              if(print.infos) cat("Folder '", i, "' will be added to the search path. \n", sep = "")
            }
          } else {
            if(is.null(install.path)) {
              #keep only the "newest" package version
              add = sort(c(.libPaths(),paths.to.add))
              add = add[!duplicated(sapply(sapply(add, \(x) tail(strsplit(x,"/")[[1]],1)), \(x) head(strsplit(x,"_")[[1]],1)), fromLast = TRUE)]
              #.libPaths(c(.libPaths()[1], add)) # .libPaths(new) replaces always to first element, in order to keep it use the construct: .libPaths(c(.libPaths(), new))
              modify_libPaths(add=add)
            } else modify_libPaths(add=install.path) #.libPaths(c(.libPaths()[1], install.path))

          }
        }

      }
      if(install==TRUE) {
        unique.highest.versions = normalizePath(.libPaths())[!duplicated(sapply(normalizePath(.libPaths()), \(x) strsplit(x,"_")[[1]][1]), fromLast = TRUE)]
        modify_libPaths(reset=unique.highest.versions)
      }

  #User specified updating
    }else {
      if(print.infos) cat("Folder '", path, "' will be added to the search path. \n", sep = "")
      #.libPaths(c(.libPaths(), path))
      modify_libPaths(add=path)
    }
}
