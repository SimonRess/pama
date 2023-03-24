#' Create an draft requirements-file
#'
#' @section Dependencies: None
#'
#' @param req.file.path (chr vector): Path in which to create the requirements file
#' @param req.file.name (chr vector): Name of the requirements-file
#' @param lists (bool): Insert a list structure into the file? TRUE=yes, FALSE=no
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

create_reqirements_file = function(req.file.path=getwd(),
                                   req.file.name="draft-requirements.txt",
                                   lists=TRUE) {

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
writeLines(txt, file.path(req.file.path,req.file.name))
}
