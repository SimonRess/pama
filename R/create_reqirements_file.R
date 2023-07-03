#' Create an draft requirements-file
#'
#' @section Dependencies: None
#'
#' @param draft (bool): [TRUE] Creating a draft with the required structure of a requirements-file. [FALSE] Building requirements file base on r-scripts within 'script.path' and packages in 'lib.path'.
#' @param req.file.path (chr value): Path in which to create the requirements file
#' @param req.file.name (chr value | NULL): Name of the requirements-file or NULL: name the file "requirements_<date>.txt"
#' @param lists (bool): Insert a list structure into the file? TRUE=yes, FALSE=no
#' @param script.path (chr vector): If draft==FALSE, provide a path to the r-scripts including the packages which  should be extracted from
#' @param patterns (chr vector): \href{https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html}{RegEx} to detect packages used in the r-scripts
#' @param lib.path (chr vector): If draft==FALSE, path(s) to the libraries the package-versions should be extracted from. NULL: Don't include package versions
#' @param ask.for.missing.versions (bool): [draft=F] Whether to ask user to enter every missing package version or not (a package version can be missing if the package is not installed within the provided 'lib.path').
#'
#' @details Create an draft requirements-file which can be filled with needed packages+versions & package-lists
#'
#' @section Side effects: Creating an draft requirements-file in the given folder
#' @section Return: None
#'
#' @export
#'
#' @keywords create requirements-file
#'
#' @examples
#' \dontrun{
#' #Creating a req-file in the working directory called "draft-requirements.txt", with list structure
#' create_reqirements_file()
#'
#' #Creating a req-file in "C:/Project" called "requirements.txt", without list structure
#' create_reqirements_file(req.file.path="C:/Project", req.file.name="requirements.txt", lists=FALSE)
#' }
#'
#' @author Simon Ress

create_reqirements_file = function(draft=TRUE,
                                   req.file.path=getwd(),
                                   req.file.name=NULL, #"requirements.txt",
                                   lists=TRUE,
                                   script.paths=NULL,
                                   patterns = c("library\\(([^,)]+)", "require\\(([^,)]+)", "library_version\\(([^,)]+)"), #old: c(".*library\\(\\s*(.*?)\\s*\\).*", ".*require\\(\\s*(.*?)\\s*\\).*"),
                                   lib.path=NULL,
                                   ask.for.missing.versions=TRUE) {


  #Check format of args
    if(!is.logical(draft)) {stop(paste0("'draft = ", draft, "' is not of type <bool>. Please provide a boolean!"))}

    if(!is.character(req.file.path)) {stop(paste0("'req.file.path = ", req.file.path, "' is not of type <character>. Please provide a character value!"))}
    if(!length(req.file.path)==1) {stop(paste0("'req.file.path = ", req.file.path, "' is not a single character value. Please provide a single character value!"))}

    if(!is.null(req.file.name)){
      if(!is.character(req.file.name)) {stop(paste0("'req.file.name = ", req.file.name, "' is not of type <character>. Please provide a character value!"))}
      if(!length(req.file.name)==1) {stop(paste0("'req.file.name = ", req.file.name, "' is not a single character value. Please provide a single character value!"))}
    }

    if(!is.logical(lists)) {stop(paste0("'lists = ", lists, "' is not of type <bool>. Please provide a boolean!"))}

    if(!is.character(script.paths)) {stop(paste0("'script.paths = ", script.paths, "' is not of type <character>. Please provide a character vector!"))}

    if(!is.character(patterns)) {stop(paste0("'patterns = ", patterns, "' is not of type <character>. Please provide a character vector!"))}

    if(!is.null(lib.path)){
      if(!is.character(lib.path)) {stop(paste0("'lib.path = ", lib.path, "' is not of type <character>. Please provide a character vector!"))}
    }

    if(!is.logical(ask.for.missing.versions)) {stop(paste0("'ask.for.missing.versions = ", ask.for.missing.versions, "' is not of type <bool>. Please provide a boolean!"))}




if(draft){

req.file.name="draft-requirements.txt"

if(lists) {
txt <- paste0("R-version 3.2.1

Packages:
dplyr 1.0.0

#vizualisation
ggplot2 2.1.0
ggmap 3.0.2

#statistics
stats 4.3.0
dplyr 0.8.0

#reporting
shiny 1.7.0
")
} else{

txt <- paste0("R-version 3.2.1

Packages:
ggplot2 2.1.0
dplyr 1.0.0
")
}

}

  #Creating a requirements-file based on the packages used in the r-scripts within the 'script.path' and version of these packages within the 'lib.path'
  if(!draft){
    if(is.null(req.file.name)) req.file.name=paste0("requirements_",Sys.Date(), ".txt") # use this name pattern if no other name is provided
    #extract packages loaded within each script
      req = list()
      for(script.path in script.paths) {
        rscripts = dir(script.path)
        rscripts = rscripts[grep("\\.r$", tolower(rscripts))] #keep only r-scripts form the folders (-> end with ".r")
        for(rscript in rscripts){
          file = suppressMessages(readLines(paste0(script.path,"/", rscript)))
          req.packages = vector()
          for(p in patterns){
            req.packages = c(req.packages,
                             as.vector(na.omit(unlist(
                               lapply(file, \(x)  gsub("\"|\'","", # Delete " and ' around package names
                                                       regmatches(x, regexec(p, x))[[1]][2] # extract package names inside patterns (here library() or require())
                                                       )
                                      )))))
          }
          req[[rscript]] = unique(req.packages)
        }
      }

      #keep only "script" with packages loaded [-> != character(0)]
        req = req[!sapply(req, \(x) identical(x, character(0)))]

      #delete \" from package names (if library("package") used, then output is \"package\"")
        req = lapply(req, \(x) lapply(x, \(x) gsub('\"', "", x)))


    if(!is.null(lib.path)){
      #extract versions of all packages loaded within each script
      #installed packages in 'lib.path'
        #extract package version only from folders installed with specific-package-version, format: <packagename>_<version>/<packagename>
          # packages = dir(lib.path)
          # names = sapply(packages, \(x) strsplit(x,"_")[[1]][1])
          # versions = sapply(packages, \(x) strsplit(x,"_")[[1]][2])
          # installed.packages = data.frame(name = names, version = versions)

      packages.paths = dir(lib.path) #folders in lib.path
      #Find actual package folder -> required if specific package version was installed -> creates folders <packagename>_<version>/<packagename>
      packages.paths = sapply(packages.paths, \(x) ifelse(grepl("_",x),
                                         file.path(lib.path, x, dir( file.path(lib.path,x))),
                                         file.path(lib.path, x)))

      versions = sapply(packages.paths, \(x) readRDS(file.path(x, "Meta/package.rds"))[["DESCRIPTION"]][["Version"]])
      names = sapply(names(versions), \(x) strsplit(x, "_")[[1]][1])
      installed.packages = data.frame(name = names, version = versions)

    #Enrich package names in 'req', by their versions provided in 'installed.packages'
      req = lapply(req, \(x) lapply(x, \(x) paste0(trimws(x),"_",installed.packages[installed.packages$name==x, "version"])))
    } else{
      # don't provide any versions if no lib.path is provided -> user will be ask for every package version
      req = lapply(req, \(x) lapply(x, \(x) paste0(trimws(x),"_")))
    }


    #Ask user to complete missing package versions
    if(ask.for.missing.versions){
      for(lists in names(req)){ #for testing: names(tail(req,1))
        print(lists)
        for(pv in 1:length(req[[lists]])) {
          #print(pv)
          #print(grepl("_$", req[[lists]][[pv]]))
          print(req[[lists]][[pv]])
          if(grepl("_$", req[[lists]][[pv]])){
            answer=""
            answer = readline(prompt = paste0("There's no version of package '", gsub("_$","",req[[lists]][[pv]]), "' found... Please provide the version (e.g. 1.0.2):"))
            while(answer==""){
              #print(paste0("There's no version of package '", req[[lists]][[pv]], "' found... Please provide the version (e.g. 1.0.2):"))
              cat("Your Entry'",answer,"' is not valid, try again... \n", sep="")
              answer = readline(prompt = paste0("There's no version of package '", gsub("_$","",req[[lists]][[pv]]), "' found... Please provide the version (e.g. 1.0.2):"))
            }
            package.version = paste0(req[[lists]][[pv]], answer)
            cat("Package-Version saved: ", package.version, "\n", sep="")
            req[[lists]][[pv]] = package.version
          }
        }
      }

    }


    #build requirements-file
    lists = character()
      for(n in names(req)){
       lists = paste(lists, paste0("#",n), sep = "\n")
       lists = paste(lists, paste0(req[[n]], collapse = "\n"), sep ="\n", collapse = "")
       lists = paste0(lists, "\n")
       cat(lists)
      }





    #creating text to write into requirements-file
    txt <- paste("R-version 3.2.1\n\nPackages:", lists, sep="", collapse = "")
    cat("Saving requirements-file:", "\n")
    cat("-----------------------------", "\n")
    cat(lists, "\n")
    cat("-----------------------------", "\n")

    cat("\n")
    cat("txt length:", (nchar(txt)), "\n")
  }


cat("Filename: ", file.path(req.file.path,req.file.name), "\n")

writeLines(txt, file.path(req.file.path,req.file.name))
}
