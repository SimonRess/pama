#' Creation of a new library folder (incl. stand. packages)
#'
#' @section Dependencies: NONE
#'
#' @description Creates a new library folder in a specified directory to which
#' all "standard R packages" are copied. This library folder can be used as a
#' stand-alone project library for package version control with the \link{pama}-package.
#' @param new.lib (chr): Path of the new library-folder to be created (e.g. "C:/Users/Simon/Documents/NewProject/lib")
#' @param main.lib.path (chr): Path of the library-folder within R Home Directory (Attention: Specify path to correct R version, if more than one exists)
#' @param standard_packages (chr vector): The standard R packages which should be taken over from the existing R installation (hint: stay with the \href{https://cran.r-project.org/doc/manuals/r-release/R-FAQ.html#Which-add_002don-packages-exist-for-R_003f}{"Add-on packages in R"}.)
#'
#' @section Side effects: (1.) Creating a new library folder in the specified directory and (2.) copying all "standard R packages" into it.
#' @section Return: NONE
#'
#' @seealso `browseVignettes("pama")`, `help(package = "pama")`
#'
#' @keywords default library folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Creating a new "lib"-folder and using the library of the running R version as the source of the standard packages to be copied
#' setupLibrary(new.lib="C:/Users/Simon/Documents/NewProject/lib")
#' }
#'
#' @author Simon Ress


setupLibrary = function(new.lib=NULL,
                        main.lib.path=paste0(R.home(),"/library"), # used library-folder within R Home Directory
                        standard_packages = c("base", "compiler", "datasets", "grDevices", "graphics",
                                              "grid", "methods", "parallel", "splines", "stats",
                                              "stats4", "tcltk", "tools", "utils")) {
#https://cran.r-project.org/doc/manuals/r-release/R-intro.html#Standard-packages
#https://cran.r-project.org/doc/manuals/r-release/R-FAQ.html#Which-add_002don-packages-exist-for-R_003f
  # - R distribution comes with the following packages (Not on CRAN): base, compiler, datasets, grDevices, graphics, grid, methods, parallel, splines, stats, stats4, tcltk, tools, utils
  # - CRAN packages included in all binary distributions of R: KernSmooth, MASS, Matrix, boot, class, cluster, codetools, foreign, lattice, mgcv, nlme, nnet, rpart, spatial, survival

  #Create lib-folder
    cat("Creating new library-folder (",new.lib,")...", "\n", sep="")
    dir.create(new.lib)

  #Add "Standard Packages"
  for(p in standard_packages){
    cat("Copying standard package '",p,"'...", "\n", sep="")
    #Get version of installed "Standard Package"
      file = readRDS(file.path(main.lib.path, p, "Meta/package.rds"))
      version = file[["DESCRIPTION"]][["Version"]]
    #Create "package-version"-folder in new lib-folder
      dir.create(file.path(new.lib,paste0(p,"_",version)))
    #Paste package-folder into new "package-version"-folder
      file.copy(paste(main.lib.path,p,sep="/"), paste(new.lib,paste0(p,"_",version),sep="/"), recursive=TRUE)
  }
}
