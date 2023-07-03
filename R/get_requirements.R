#' Get data from requirements-file
#' @section Dependencies: NONE
#'
#' @rdname get_requirements
#'
#' @param req.file.name (chr vector): Name of the requirement-file
#' @param req.file.path (chr vector): Folder of the requirement-file
#'
#' @details Returns the information of the requirements-file
#'
#' @section Side effects: None
#' @section Return: List of lists. First list ('r.version') returns the required r-version. Other list (e.g. 'main') return vectors including required versions of packages in format: "<package_name> <version>"
#' @export
#'
#' @keywords requirements-file
#' @seealso \code{\link[base]{.libPaths}}
#'
#' @examples
#' \dontrun{
#' # Load requirements specifies in "requirements.txt" within the working directory
#' get_requirements()
#' }
#'
#' @author Simon Ress

get_requirements = function(req.file.name="requirements.txt",
                            req.file.path=getwd()) {
  # Reads the requirements-file and outputs a list of packages to install/load
  # :param req.file.name (chr vector): Name of the requirements-file (-> USE .txt-file !!!)
  # :param req.file.path (chr vector): Path to the requirements-file
  # :return: List of list including vectors with packages to install/load: <package_name> <version>
  # :side-effects: none

  #Check format of args
    if(!is.character(req.file.name)) {stop(paste0("'req.file.path = ", req.file.name, "' is not of type <character>. Please provide a character value!"))}
    if(!length(req.file.name)==1) {stop(paste0("'req.file.path = ", req.file.name, "' is not a single character value. Please provide a single character value!"))}

    if(!is.character(req.file.path)) {stop(paste0("'req.file.path = ", req.file.path, "' is not of type <character>. Please provide a character value!"))}
    if(!length(req.file.path)==1) {stop(paste0("'req.file.path = ", req.file.path, "' is not a single character value. Please provide a single character value!"))}



  req = scan(paste0(req.file.path,"/",req.file.name), what="", sep="\n", quiet = T)
  r.version = sub("R-version ", "",req[grep("R-version", req)])
  out = list(r.version = r.version)
  packages = req[grep("Packages:", req)+1:length(req)]
  packages = packages[!is.na(packages)]
  lists = grep("#", packages)

  if(length(lists)==0) { #Take all packages
    out[["main"]] = packages
  } else { # assign packages to lists
    out[["main"]] = packages[1:grep("#", packages)[1]-1]
    out[["main"]] = ifelse(identical(character(0),out[["main"]]), NA, out[["main"]])
    for(i in grep("#", packages)){
      l = packages[(i+1):length(packages)]
      out[[packages[i]]] = if(any(grep("#", l))) l[1:grep("#", l)[1]-1] else l[1:length(l)]
    }
  }

  return(out)
}
