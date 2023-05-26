#' Adjust package-loading in R scripts to pama-style
#'
#' @section Dependencies:
#' - get_requirements()
#'
#' @param req.file.path (chr vector): Path in which to create the requirements file
#' @param req.file.name (chr vector): Name of the requirements-file
#' @param lists (bool): Insert a list structure into the file? TRUE=yes, FALSE=no
#' @param script.paths (chr vector): Provide a path / paths to the R scripts that should be adjusted
#' @param patterns (chr vector): \href{https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html}{RegEx} to detect package-loadings in the r-scripts
#'
#' @details Adjusting the package-loading in R scripts to pama-style
#'
#' @section Side effects: Adjusting the package-loading in R scripts to pama-style
#' @section Return: None
#'
#' @export
#'
#' @keywords pama-style, library_version
#'
#' @examples
#' \dontrun{
#' #Creating a req-file in the working directory called "draft-requirements.txt", with list structure
#' apply_library_version()
#'
#' #Adjust package-loading within R scripts in .\Scripts to pama-style, using "requirements.txt" within .\
#' apply_library_version(req.file.path = r"(C:\Users\sress\Desktop\titanic-r-master)",
#'                       req.file.name = "requirements.txt",
#'                       script.paths = r"(C:\Users\sress\Desktop\titanic-r-master\Scripts)")
#' }
#'
#' @author Simon Ress

apply_library_version = function(req.file.path=getwd(),
                                 req.file.name="requirements.txt",
                                 lists=TRUE,
                                 script.paths=NULL,
                                 patterns = c("library\\(([^,)]+)\\)", "require\\(([^,)]+)\\)")) {

  if(is.null(script.paths)){
    warning("No 'script.paths' entered. Set 'script.paths' in order to make apply_library_version() work!")
    return()
  }

  if(!isTRUE(lists)){
    warning("Until now only list=TRUE is supported!")
    return()
  }

  #req.file.path=r"(C:\Users\sress\Desktop\titanic-r-master)"
  #req.file.name="requirements.txt"
  #script.paths=r"(C:\Users\sress\Desktop\titanic-r-master\Scripts)"

  #Check if Code inherits pattern to replace and do it
  library_to_library_version = function(old_code){
    if(grepl(paste(patterns, collapse = "|"), old_code)){
      x = old_code
      #extract the matches from within the code, e.g "library(ggplot2)"
        .matches = regmatches(x, gregexpr(paste(patterns, collapse = "|"), x))[[1]]

      #extract the applied version from the requirements-file
        .req = get_requirements(file.name = req.file.name, path = req.file.path)[[paste0("#",rscript)]] #take package versions from the accurate list/RScript

      #
      new_code = old_code
      for(.m in .matches){
        #print(.m)
        #.package = gsub("'|\"", "", sapply(.m, \(.x) regmatches(.x, gregexec(paste(patterns, collapse = "|"), .x))[[1]][2]))
        .package = gsub("'|\"", "", sapply(.m, \(.x) regmatches(.x, gregexec(paste(patterns, collapse = "|"), .x))[[1]][-1][nzchar(regmatches(.x, gregexec(paste(patterns, collapse = "|"), .x))[[1]][-1])]))

        #print(.package)
        .version = strsplit(.req[grepl(.package, .req)], "_")[[1]][2]
        #print(.version)
        .escaped = gsub("([[:punct:]])", "\\\\\\1", .m) # escape all special characters in the match-pattern, else they will be used as Regex functionalities
        new_code = sub(.escaped, paste0("library_version('",.package, "','", .version,"')"), new_code)
        #print(new_code)
      }

      return(new_code) #return adjusted code if pattern/s to replace was/were found
    }

    return(old_code) #return old code if no pattern to replace was found

  }

  #testing
    #library_to_library_version("library(ggplot2); aa")

    #unlist(lapply(file, \(x) gsub(patterns[1], library_to_library_version(x), x)))
    #unlist(lapply(file, \(x) library_to_library_version(x)))

  for(script.path in script.paths) {
    rscripts = dir(script.paths)
    #requirements = get_requirements(file.name = req.file.name, path = req.file.path)

    #Loop over R-scripts
    for(rscript in rscripts){
      file = suppressMessages(readLines(paste0(script.path,"/", rscript)))
      txt = unlist(lapply(file, \(x) library_to_library_version(x)))
      writeLines(txt, file.path(script.paths,rscript))
    }
  }

}


