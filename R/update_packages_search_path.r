#' Updating Search Paths for Packages
#' @section Dependencies: None
#'
#' @rdname update_packages_search_path
#'
#' @param path - (chr vector): Location of R library tree which should be added to the seach path
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

update_packages_search_path = function(path = NULL, install = FALSE) {
  # Dependencies: NONE
  #
  # Updates by default the "Search Paths for Packages" (-> '.libPaths()') by searching for folders with name <package_name>_<version> and adding these as search paths
  # Also a vector of paths with should be added to the search list can be specified
  # :path (chr vector): Paths be added to the search list, e.g "C:/Users/simon/AppData/Local/R/win-library/4.2/ggplot2_3.1.1" or c(<path1>, <path2>)
  # :return: None
  # :side-effects: Updating Search Paths for Packages, see .libPaths()

  #Default updating of search paths by adding "package-version"-folders
    if(is.null(path)) {
      for(p in .libPaths()) {
        package = dir(p)[which(grepl("_", dir(p)))]
        if(install==TRUE) package = package[!duplicated(sapply(package, \(x) strsplit(x,"_")[[1]][1]), fromLast = TRUE)]
        if(length(package)>=1){
          paths.to.add = paste0(p, "/", package)
          for(i in paths.to.add) {
            cat("Folder '", i, "' will be added to the search path. \n", sep = "")
          }
          if(install==FALSE){
            .libPaths(c(.libPaths(), paths.to.add)) # .libPaths(new) replaces always to first element, in order to keep it use the construct: .libPaths(c(.libPaths(), new))
          } else {
            .libPaths(c(.libPaths()[1], paths.to.add)) # .libPaths(new) replaces always to first element, in order to keep it use the construct: .libPaths(c(.libPaths(), new))
          }
        }

      }

  #User specified updating
    }else {
      cat("Folder '", path, "' will be added to the search path. \n", sep = "")
      .libPaths(c(.libPaths(), path))
    }
}
