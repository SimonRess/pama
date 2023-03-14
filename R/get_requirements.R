#' ...
#' @section Dependencies: NONE
#'
#' @rdname get_requirements
#'
#' @param file.name (chr vector): ...
#' @param path (chr vector): ...
#'
#' @details test
#'
#' @section Side effects: ...
#' @section Return: ...
#' @export
#'
#' @keywords ...
#' @seealso \code{\link[utils]{.libPaths()}}
#'
#' @examples
#' \dontrun{
#' # Load requirements specifies in "requirements.txt" within the working directory
#' get_requirements()
#' }
#'
#' @author Simon Ress

get_requirements = function(file.name="requirements.txt", path=getwd()) {
  # Reads the requirements-file and outputs a list of packages to install/load
  # :param file.name (chr vector): Name of the requirements-file (-> USE .txt-file !!!)
  # :param path (chr vector): Path to the requirements-file
  # :return: List of list including vectors with packages to install/load: <package_name> <version>
  # :side-effects: none
  req = scan(paste0(path,"/",file.name), what="", sep="\n", quiet = T)
  r.version = sub("R-version ", "",req[grep("R-version", req)])
  out = list(r.version = r.version)
  packages = req[grep("Packages:", req)+1:length(req)]
  packages = packages[!is.na(packages)]
  lists = grep("#", packages)

  if(length(lists)==0) { #Take all packages
    out[["main"]] = packages
  } else { # assign packages to lists
    #out = list(main = packages[1:grep("#", packages)[1]-1])
    out[["main"]] = packages[1:grep("#", packages)[1]-1]
    out[["main"]] = ifelse(identical(character(0),out[["main"]]), NA, out[["main"]])
    for(i in grep("#", packages)){
      l = packages[(i+1):length(packages)]
      out[[packages[i]]] = if(any(grep("#", l))) l[1:grep("#", l)[1]-1] else l[1:length(l)]
    }
  }

  return(out)
}
