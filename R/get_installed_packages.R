#' Returns installed packages and their versions
#' @section Dependencies: - update_packages_search_path()
#'
#' @param lib.search.path (chr vector): Paths to search for packages, by default .libPaths() <- Internal search paths of R
#'
#' @section Side effects: None
#' @section Return: List of lists with structure c(<package_name>, <version>)
#' @export
#'
#' @keywords Returns installed packages and versions
#' @seealso \code{\link[base]{.libPaths}}
#'
#' @examples
#' get_installed_packages()
#'
#' get_installed_packages("C:/Program Files/R/R-4.2.2/library")
#'
#' @author Simon Ress

get_installed_packages = function(lib.search.path=.libPaths()) {

  #Check format of args
    if(!is.character(lib.search.path)) {stop(paste0("'lib.search.path = ", lib.search.path, "' is not of type <character>. Please provide a character vector!"))}


  #update search path by adding "package-version"-folders
  update_packages_search_path()

  #names = list()
  names = data.frame(name = NULL, version = NULL)
  # names = data.frame()
  for(pp in .libPaths()) {
    #cat("--------------------------", "\n")
    #cat("pp: ", pp, "\n")
    package.names = dir(pp)[which(!grepl("_", dir(pp)))] # don't use dir(pp), it also keeps folder names with structure <package_name>_<version>, but these folders are added seperatly by update_packages_search_path() to the search list  (-> '.libPaths()')
    for(pn in package.names){
      #cat("pn: ", pn, "\n")
      verify = suppressWarnings(try(description <- as.data.frame(read.dcf(paste0(pp, "/", pn, "/DESCRIPTION"))), silent = TRUE))
      #cat("verify: ", verify[[1]], "\n")
      if(!("try-error" %in% class(verify))){
        name = description$Package
        version = description$Version
        # date = description$Date
        #names = append(names, list(c(name, version)))#
        #names = rbind(names, c(name, version))
        names = rbind(names, data.frame(name = name, version = version))
        #names = ifelse(nrow(names)==0, data.frame(name = name, version = version), rbind(names, data.frame(name, version)))
        #cat("names:", as.vector(names), "\n")
      } else cat("There's an error with package '", pn, "'. No 'DESCRIPTION'-file could be detected within its folder. \n", sep="")
    }
  }
  #Sort by "name" and "version"
  names = names[order(names$name, names$version),]
  return(names)
}
