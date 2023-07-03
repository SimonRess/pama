#' Modify the Search Paths for Packages
#'
#' @section Dependencies: NONE
#'
#' @description Modify the "Search Paths for Packages" (.lib.loc in .libPaths-env.),
#' by (1) adding new directories to the search paths or (2) resetting the search paths
#' to a vector of directories (and deleting all previous directories).
#'
#' @param add (chr vector | NULL): Directories to be added to the search paths. NULL: add nothing
#' @param reset (chr vector | NULL): Directories to which the search paths are reset. NULL: don't reset
#'
#' @section Side effects: Modify the "Search Paths for Packages" (.lib.loc in .libPaths-env.) as specified.
#' @section Return: NONE
#'
#' @seealso `browseVignettes("pama")`, `help(package = "pama")`
#'
#' @keywords default library folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Adding the folder of dplyr version 1.1.2 to the search paths
#' modify_libPaths(add="C:/Users/<User>/Documents/NewProject/lib/dplyr_1.1.2")
#'
#' #Resetting the search paths to a vector of directories
#' modify_libPaths(reset=c("C:/Users/<User>/Documents/NewProject/lib1/", "C:/Users/<User>/Documents/NewProject/lib2"))
#' }


modify_libPaths = function(add = NULL, reset = NULL){

  #Check format of args
    if(!is.null(add)){
      if(!is.character(add)) {stop(paste0("'add = ", add, "' is not of type <character>. Please provide a character vector!"))}
    }

    if(!is.null(reset)){
      if(!is.character(reset)) {stop(paste0("'reset = ", reset, "' is not of type <character>. Please provide a character vector!"))}
    }


  #Adding new search path to existing paths
  if(!is.null(add)) {
    paths = unique(c(.libPaths(),add))
    paths = paths[dir.exists(paths)]
    assign(".lib.loc", paths, envir = environment(.libPaths))
  }

  #resetting .lib.loc, keep only the new paths
  if(!is.null(reset)) {
    paths = unique(reset)
    paths = paths[dir.exists(paths)]
    assign(".lib.loc", paths, envir = environment(.libPaths))
    .Library.site = paths[1]
    Sys.setenv(HOME=paths[1])

    update_packages_search_path() # adding all <package_version> folders from within the given path to .libPaths()
  }
}
