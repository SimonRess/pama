#' Adjust package-loading in R scripts to the "pama-style"
#'
#' @section Dependencies:
#' - get_requirements()
#'
#' @param req.file.path (chr value): Path in which to create the requirements file
#' @param req.file.name (chr value): Name of the requirements-file
#' @param lists (bool): Insert a list structure into the file? TRUE=yes, FALSE=no
#' @param script.paths (chr vector): Provide a path / paths to the R scripts that should be adjusted
#' @param patterns (chr vector): \href{https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html}{RegEx} to detect package-loadings in the r-scripts
#'
#' @details Adjusting the package-loading in R scripts to the "pama-style". It replaces all functions defined in `<patterns>` (default: `library()` and `require()`) by the pama-function
#' `library_version(<package_name>, <package_version>)`. The `<package_name>` attribute is extracted from the scripts. The `<package_version>` is read from the specified requirements-file
#'
#' @section Side effects: Adjusting the package-loading in R scripts to the "pama-style"
#' @section Return: None
#'
#' @export
#'
#' @keywords pama-style, library_version
#'
#' @examples
#' \dontrun{
#'
#' #Adjust package-loading within R scripts in .\Scripts to pama-style, using versions from the "requirements.txt"-file
#' apply_library_version(req.file.path = r"(C:\Users\<User>\Desktop\titanic-r-master)",
#'                       req.file.name = "requirements.txt",
#'                       script.paths = r"(C:\Users\<User>\Desktop\titanic-r-master\Scripts)")
#' }
#'
#' @author Simon Ress

apply_library_version = function(req.file.path=getwd(),
                                 req.file.name="requirements.txt",
                                 lists=TRUE,
                                 script.paths=NULL,
                                 patterns = c("library\\(([^,)]+)\\)", "require\\(([^,)]+)\\)")) {

  #Check format of args
    if(!is.character(req.file.path)) {stop(paste0("'req.file.path = ", req.file.path, "' is not of type <character>. Please provide a character value!"))}
    if(!length(req.file.path)==1) {stop(paste0("'req.file.path = ", req.file.path, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(req.file.name)) {stop(paste0("'req.file.name = ", req.file.name, "' is not of type <character>. Please provide a character value!"))}
    if(!length(req.file.name)==1) {stop(paste0("'req.file.name = ", req.file.name, "' is not a single character value. Please provide a single character value!"))}

    if(!is.logical(lists)) {stop(paste0("'lists = ", lists, "' is not of type <bool>. Please provide a boolean!"))}

    if(!is.character(script.paths)) {stop(paste0("'script.paths = ", script.paths, "' is not of type <character>. Please provide a character vector!"))}

    if(!is.character(patterns)) {stop(paste0("'patterns = ", patterns, "' is not of type <character>. Please provide a character vector!"))}


  #Check if Code inherits pattern to replace and do it
  library_to_library_version = function(old_code){
    if(grepl(paste(patterns, collapse = "|"), old_code)){
      x = old_code
      #extract the matches from within the code, e.g "library(ggplot2)"
        .matches = regmatches(x, gregexpr(paste(patterns, collapse = "|"), x))[[1]]

      #extract the applied version from the requirements-file
        .req = get_requirements(req.file.name = req.file.name, req.file.path = req.file.path)[[paste0("#",rscript)]] #take package versions from the accurate list/RScript

        #Check whether script-to-adjust is part of req-file or not
          if(is.null(.req)) stop("There is no information about the script '", rscript, "' within the req-file ('",file.path(req.file.path,req.file.name),"'). Please add a ",paste0("#",rscript) ," section to the req-file, e.g. by running create_reqirements_file() with appropriate settings.")


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


  for(script.path in script.paths) {
    rscripts = dir(script.paths)

    #Loop over R-scripts
    for(rscript in rscripts){
      file = suppressMessages(readLines(paste0(script.path,"/", rscript)))
      txt = unlist(lapply(file, \(x) library_to_library_version(x)))
      writeLines(txt, file.path(script.paths,rscript))
    }
  }

}


