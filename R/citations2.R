#' Creation of a 'citation table' for packages
#' @section Dependencies: get_requirements()
#'
#' @rdname citations2
#'
#' @param use.req.file (bool): Whether (TRUE) to use a requirements-file as source of packages to be cited or (FALSE) to provide package name in 'packages'-attribute
#' @param req.file.name (chr value): Name of the requirements-file
#' @param req.file.path (chr value): Path of the requirements-file
#' @param packages (chr vector): If requirements-file is not used, a vector with the names of the packages to be cited
#' @param lib.loc (bool | chr vector): path names of R libraries, or the directory containing the source for package, or NULL. The default value of NULL corresponds to all libraries currently known. If the default is used, the loaded packages are searched before the libraries.
#' @param output.path (chr value): Where to save the csv-file containing the citation table
#'
#' @details Creating a citation table the given packages including the columns 'Package', 'Version' and 'Zitat'.
#'
#' @section Side effects: Saving the citation table for packages as .csv-file within the output.path. File name: paste0("/lib_citations_",Sys.Date(),".csv")
#' @section Return: Data frame including the citation table for packages
#' @export
#'
#' @keywords citation packages
#' @seealso \code{\link[pama]{get_requirements}}
#'
#' @examples
#' \dontrun{
#' # Citing the packages from the "requirements.txt" within the current working directory
#' citations2()
#'
#' # Citing the packages from the "req.txt" within "C:/GIT/Packages/PaMaTo" and saving ourput within the current working directory
#' citations2(req.file.path = "C:/GIT/Packages/PaMaTo",
#'            req.file.name = "req.txt")
#' }
#'
#' @author Simon Ress
#'

citations2 = function(use.req.file = TRUE,
                      req.file.name = "requirements.txt",
                      req.file.path = getwd(),
                      packages = c("ggplot2", "dplyr"),
                      lib.loc = NULL,
                      output.path = getwd()){

  #Check format of args
    if(!is.logical(use.req.file)) {stop(paste0("'use.req.file = ", use.req.file, "' is not of type <bool>. Please provide a boolean!"))}

    if(!is.character(req.file.name)) {stop(paste0("'req.file.name = ", req.file.name, "' is not of type <character>. Please provide a character value!"))}
    if(!length(req.file.name)==1) {stop(paste0("'req.file.name = ", req.file.name, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(req.file.path)) {stop(paste0("'req.file.path = ", req.file.path, "' is not of type <character>. Please provide a character value!"))}
    if(!length(req.file.path)==1) {stop(paste0("'req.file.path = ", req.file.path, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(packages)) {stop(paste0("'packages = ", packages, "' is not of type <character>. Please provide a character vector!"))}

    if(!(is.null(lib.loc) | is.character(lib.loc))) {stop(paste0("'lib.loc = ", lib.loc, "' is not NULL or of type <character>. Please provide a character vector or set it to NULL!"))}

    if(!is.character(output.path)) {stop(paste0("'output.path = ", output.path, "' is not of type <character>. Please provide a character value!"))}
    if(!length(output.path)==1) {stop(paste0("'output.path = ", output.path, "' is not a single character value. Please provide a single character value!"))}


  #Set lib.loc
  if(is.null(lib.loc)) lib.loc = .libPaths() # default -> .libPaths()
  if(!is.null(lib.loc)) lib.loc = lib.loc

  #Extracting packages from a requirements-file
  if(use.req.file){
    #Create citation text by hand
      # name = sapply(unlist(get_requirements()[[-1]]), \(x) strsplit(x, " ")[[1]][1])
      # version = sapply(unlist(get_requirements()[[-1]]), \(x) strsplit(x, " ")[[1]][2])
      # bref = lapply(name, citation)
      # author = bref[[2]]$author
      # year = bref[[2]]$year
      # title = bref[[2]]$title
      # organization = bref[[2]]$organization
      # address = bref[[2]]$address
      # text = paste0(author," (", year, "). ", title,". ", organization, ", ", address)

    #Auto-creation of citation text
      name = c("base", sapply(unlist(get_requirements(req.file.name = req.file.name, req.file.path = req.file.path)[-1]), \(x) strsplit(x, " ")[[1]][1]))
      name = na.omit(name)
      #text = unlist(lapply(name, \(x) paste0(capture.output(print(citation(x), style = "text")), collapse = " ")))
      text = unlist(lapply(name, \(x)
                           paste0(capture.output(
                             print(
                               citation(package = strsplit(x, "_")[[1]][1],
                                        lib.loc = .libPaths()[grepl(x, .libPaths())]),
                               style = "text")),
                             collapse = " ")))
      version = c(unlist(get_requirements(req.file.name = req.file.name, req.file.path = req.file.path)[1]), sapply(na.omit(unlist(get_requirements(req.file.name = req.file.name, req.file.path = req.file.path)[-1])), \(x) strsplit(x, "_")[[1]][2]))

  }
  #Use provided package names from the 'packages'-attribute
  if(!use.req.file){
    name = packages
    text = unlist(lapply(name, \(x) paste0(capture.output(print(citation(x), style = "text")), collapse = " ")))
    version = unlist(lapply(name, \(x) as.character(packageVersion(x))))
  }

  #Create table to export & save
    table = data.frame(Package = name, Version = version, Zitat = text)
    table = unique(table) #keep only one row per package-version -> required because package-version can be listed often in reqirements-file
    row.names(table) <- NULL # deleting row names


  #Save table as .csv-file
    cat("Writing citation text into '", paste0(output.path, paste0("/lib_citations_",Sys.Date(),".csv")), "'.", "\n", sep="")
    write.csv2(table, paste0(output.path, paste0("/lib_citations_",Sys.Date(),".csv")),
               row.names = FALSE,
               fileEncoding = "latin1")

    return(table)
}




