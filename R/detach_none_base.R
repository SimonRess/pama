#' Detach all none base packages
#' @section Dependencies: NONE
#'
#' @rdname detach_none_base
#'
#' @param None Always all non base packages (except of package:PaMa itself) will be detached
#'
#' @description Detach all (none base) packages -> start with empty package-list
#'
#' @details Detaches all packages listed in utils::sessionInfo()$otherPkgs except of package:pama
#'
#' @section Side effects: Detaches all (none base) packages
#' @section Return: vector of attached none base packages -> is empty ("") if it worked
#' @export
#'
#' @keywords detach non-base packages namespace
#' @seealso \code{\link[base]{.libPaths}}
#'
#' @examples
#' \dontrun{
#' # Load requirements specifies in "requirements.txt" within the working directory
#' detach_none_base()
#' }
#'
#' @author Simon Ress (simon-ress.de)

detach_none_base = function(None){
  utils::capture.output(suppressWarnings(lapply(paste('package:',names(utils::sessionInfo()$otherPkgs),sep=""),
                                         \(x) if(x!='package:pama') try(detach(x, character.only=TRUE,unload=TRUE,force = TRUE),silent = T))),
                        file='NUL')

  attached = sapply(utils::sessionInfo()$otherPkgs, \(x) x[c("Package", "Version")])
  attached = ifelse(identical(list(),attached), "",
                    as.vector(apply(sapply(sessionInfo()$otherPkgs, \(x) x[c("Package", "Version")]), 2, \(x) paste(x, collapse = "_"))))
  return(attached)
}
