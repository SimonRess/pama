#' Updating Search Paths for Packages
#' @section Dependencies: None
#'
#' @rdname update_packages_search_path
#'
#' @param path (chr vector): Location of R library tree which should be added to the search path
#' @param print.infos (bool): Regulates output in the users console. TRUE: All internal information will be printed (useful for debugging). FALSE (by default): No or much less information will be printed.
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

update_packages_search_path = function(path = NULL, install = FALSE, install.path = NULL, print.infos = FALSE) {
  # Dependencies: NONE
  #
  # Updates by default the "Search Paths for Packages" (-> '.libPaths()') by searching for folders with name <package_name>_<version> and adding these as search paths
  # Also a vector of paths with should be added to the search list can be specified
  # :path (chr vector): Paths be added to the search list, e.g "C:/Users/simon/AppData/Local/R/win-library/4.2/ggplot2_3.1.1" or c(<path1>, <path2>)
  # :return: None
  # :side-effects: Updating Search Paths for Packages, see .libPaths()

  #Default updating of search paths by adding "package-version"-folders
    if(is.null(path)) {
      if(dir.exists(file.path(getwd(), "lib"))) lib.paths = c(.libPaths(), file.path(getwd(), "lib")) else lib.paths = .libPaths() # always ALSO search in "lib" in project-folder
      for(p in lib.paths) {
        package = dir(p)[which(grepl("_", dir(p)))] # greps folders which contains a "_" -> e.g "...\ggplot2_3.1.0"
        if(install==TRUE) package = package[!duplicated(sapply(package, \(x) strsplit(x,"_")[[1]][1]), fromLast = TRUE)] #keep only the "newest" package version
        if(length(package)>=1){
          paths.to.add = paste0(p, "/", package)
          if(install==FALSE){
            .libPaths(c(.libPaths(), paths.to.add)) # .libPaths(new) replaces always to first element, in order to keep it use the construct: .libPaths(c(.libPaths(), new))
            for(i in paths.to.add) {
              if(print.infos) cat("Folder '", i, "' will be added to the search path. \n", sep = "")
            }
          } else {
            if(is.null(install.path)) {
              #keep only the "newest" package version
              add = sort(c(.libPaths(),paths.to.add))
              add = add[!duplicated(sapply(sapply(add, \(x) tail(strsplit(x,"/")[[1]],1)), \(x) head(strsplit(x,"_")[[1]],1)), fromLast = TRUE)]
              .libPaths(c(.libPaths()[1], add)) # .libPaths(new) replaces always to first element, in order to keep it use the construct: .libPaths(c(.libPaths(), new))
            } else .libPaths(c(.libPaths()[1], install.path))

          }
        }

      }

  #User specified updating
    }else {
      if(print.infos) cat("Folder '", path, "' will be added to the search path. \n", sep = "")
      .libPaths(c(.libPaths(), path))
    }
}
