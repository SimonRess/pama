#' Creation of a 'citation table' for packages
#' @section Dependencies: get_requirements()
#'
#' @rdname citations2
#'
#' @param use.requirements.file Whether (TRUE) to use a requirements-file as source of packages to be cited or (FALSE) to provide package name in 'packages'-attribute
#' @param requirements.file.name Name of the requirements-file
#' @param requirements.file.path Path of the requirements-file
#' @param packages If requirements-file is not used: Vector with the names of the packages to be cited
#' @param output.path Where to save the csv-file containing the citation table
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
#' citations2(requirements.file.path = "C:/GIT/Packages/PaMaTo",
#'            requirements.file.name = "req.txt")
#' }
#'
#' @author Simon Ress
#'

citations2 = function(use.requirements.file = TRUE,
                      requirements.file.name = "requirements.txt",
                      requirements.file.path = getwd(),
                      packages = c("ggplot2", "dplyr"),
                      output.path = getwd()){

  #Extracting packages from a requirements-file
  if(use.requirements.file){
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
      name = c("base", sapply(unlist(get_requirements(file.name = requirements.file.name, path = requirements.file.path)[-1]), \(x) strsplit(x, " ")[[1]][1]))
      name = na.omit(name)
      #text = unlist(lapply(name, \(x) paste0(capture.output(print(citation(x), style = "text")), collapse = " ")))
      text = unlist(lapply(name, \(x)
                           paste0(capture.output(
                             print(
                               citation(package = strsplit(x, "_")[[1]][1],
                                        lib.loc = .libPaths()[grepl(x, .libPaths())]),
                               style = "text")),
                             collapse = " ")))
      version = c(unlist(get_requirements(file.name = requirements.file.name, path = requirements.file.path)[1]), sapply(na.omit(unlist(get_requirements(file.name = requirements.file.name, path = requirements.file.path)[-1])), \(x) strsplit(x, "_")[[1]][2]))

  }
  #Use provided package names from the 'packages'-attribute
  if(!use.requirements.file){
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




