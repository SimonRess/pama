#'  Build the URL of package-versions
#'
#' @section Dependencies: None
#'
#' @description Creates the URL of a package file for installation.
#' This URL build on the given URL-components (`.cran.mirror`, `.main.path` & `.archiv.path`),
#' the packages information (`.package` & `.version`), the packages storage location in archive or main (`.main`) and
#' the type of given CRAN repo (`.repo`).
#'
#' @param .main (bool):
#' TRUE (default) = Build the URL of an current package version in the repo, `.main.path` is used.
#' FALSE = TRUE=Build the URL of an older package version in the repos archive, `.archive.path` is used.
#' @param .repo (chr value): Which type of repo is used? This information is utilized when building the URL,
#' because each repo has its own file structure. Currently supported: "cran" & "nexus"
#' @param .overview (bool):
#' FALSE (default)= Return link to package-version src (.tar.gz ) or bin (.zip) file
#' TRUE=Return page with overview of all versions of a package (e.g.: https://cloud.r-project.org/src/contrib/Archive/dplyr/)
#' @param .package (chr value): Name of the package to install (e.g. "ggplot2")
#' @param .version (chr value): Version of the package to install (e.g. "3.4.0")
#' @param .cran.mirror (chr value): Main url of the repo to use (e.g. "https://cloud.r-project.org/")
#' @param .main.path (chr value): URL-path to the pages main page of the repo to use (e.g. "src/contrib/"")
#' @param .archiv.path (chr value): URL-path to the archive of the repo to use (e.g. "src/contrib/Archive/")
#'
#' @section Side effects: None
#' @section Return: (chr value) URL of the package version
#' @export
#'
#' @note Here the structure of cran:
#' Packages *main* page: https://cran.r-project.org/web/packages/dplyr/index.html
#' Packages *main* storage location: https://cran.r-project.org/src/contrib/dplyr_1.1.2.tar.gz <-
#' Packages *archive* page: https://cran.r-project.org/src/contrib/Archive/dplyr/
#' Packages *archive* storage location: https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_1.1.0.tar.gz
#'
#' @seealso `browseVignettes("pama")`, `help(package = "pama")`
#' @keywords cran URL package-versions
#'
#' @examples
#' \dontrun{
#' # Settings:
#' .package = "dbplyr"
#' .version = "2.3.1"
#' .cran.mirror = "https://cran.r-project.org/"
#' .main.path = "src/contrib/"
#' archiv.path = "src/contrib/Archive/"
#'
#' # Build an URL of a current package version on CRAN
#' build_package_url(.repo = "cran")
#'    #Output should be: "https://cran.r-project.org/src/contrib/dbplyr_2.3.1.tar.gz"
#'
#' # Build an URL of the CRAN archive
#' build_package_url(.main=FALSE, .repo = "cran")
#'    # Output should be: "https://cran.r-project.org/src/contrib/Archive/dbplyr/dbplyr_2.3.1.tar.gz"
#'
#' # Build an URL of the "nexus" repo type
#' .repo = "nexus"
#' .cran.mirror = "https://<***>.de/service/rest/repository/browse/r-public/"
#' .archiv.path = "bin/windows/contrib/"
#' build_package_url(.repo = repo)
#'    # Output should be: "https://<***>.de/service/rest/repository/browse/r-public/bin/windows/contrib/4.2/dbplyr/2.3.1/dbplyr_2.3.1.zip"
#'
#' # Test installation of a package with the created URL
#' .cran.mirror = "https://cran.r-project.org/"
#' .main.path = "src/contrib/"
#' .url = build_package_url(.main=T, .repo = "cran", .package="gt", .version="0.9.0")
#' install.packages(url, repos=NULL, type="source")
#' library(gt)
#' remove.packages("gt")
#'
#' }
#'
#' @author Simon Ress

build_package_url = function(.main = TRUE, .repo = parent.frame()$repo, .overview = F,
                    .package=parent.frame()$package,
                    .version=parent.frame()$version,
                    .cran.mirror = parent.frame()$cran.mirror,
                    .main.path = parent.frame()$main.path,
                    .archiv.path = parent.frame()$archiv.path){
  # cran.mirror = "https://cran.r-project.org/"
  # archiv.path = "src/contrib/Archive/"
  # main.path = "src/contrib/"
  # package = "dbplyr"
  # version = "2.3.1"

  # CRAN main: https://cran.r-project.org/web/packages/dplyr/index.html
    # Exemplary url: https://cran.r-project.org/src/contrib/dplyr_1.1.2.tar.gz
  # CRAN archive: https://cran.r-project.org/src/contrib/Archive/dplyr/
    ## Exemplary url: https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_1.1.0.tar.gz

  # Check args
    if(!is.logical(.main)) stop(paste0(".main='",.main,"' is not a booleon. Please provide TRUE or FALSE."))
    if(length(.main)!=1) stop(paste0(".main' is not of length==1. Provide exactly one bool!"))

    if(is.character(.repo)){
      if(length(.repo)!=1) stop(paste0(".repo' is not of length==1. Provide exactly one repo name!"))
      if(!(.repo == "cran" | .repo == "nexus")) stop(paste0(".repo='",.repo,"' is not an supported option."))
    } else stop(paste0(".repo' is not an character value. Provide an character value!"))

    if(!is.logical(.overview)) stop(paste0(".overview='",.overview,"' is not a booleon. Please provide TRUE or FALSE."))
    if(length(.overview)!=1) stop(paste0(".overview' is not of length==1. Provide exactly one bool!"))

    if(is.character(.package)){
      if(length(.package)!=1) stop(paste0(".package' is not of length==1. Provide exactly one package name!"))
    } else stop(paste0(".package' is not an character value. Provide an character value!"))

    if(is.character(.version)){
      if(length(.version)!=1) stop(paste0(".version' is not of length==1. Provide exactly one package version!"))
    } else stop(paste0(".version' is not an character value. Provide an character value!"))

    if(is.character(.cran.mirror)){
      if(length(.cran.mirror)!=1) stop(paste0(".cran.mirror' is not of length==1. Provide exactly one package version!"))
    } else stop(paste0(".cran.mirror' is not an character value. Provide an character value!"))

    if(is.character(.main.path)){
      if(length(.main.path)!=1) stop(paste0(".main.path' is not of length==1. Provide exactly one package version!"))
    } else stop(paste0(".main.path' is not an character value. Provide an character value!"))

    if(is.character(.archiv.path)){
      if(length(.archiv.path)!=1) stop(paste0(".archiv.path' is not of length==1. Provide exactly one package version!"))
    } else stop(paste0(".archiv.path' is not an character value. Provide an character value!"))



  # Build url
    if(.repo=="cran"){
      if(!.overview){
        if(.main){
          return(paste0(.cran.mirror, .main.path, "/", paste0(.package,"_",.version), ".tar.gz")) #e.g. https://cran.r-project.org/ + src/contrib/ + dplyr_1.1.2.tar.gz
        }
        if(!.main){
          return(paste0(.cran.mirror, .archiv.path, .package, "/", paste0(.package,"_",.version), ".tar.gz")) #e.g. https://cran.r-project.org/ + src/contrib/Archive/ + dplyr/ + dplyr_1.1.0.tar.gz
        }
      } else {
        return(file.path(.cran.mirror, .archiv.path, .package)) # e.g. https://cloud.r-project.org/src/contrib/Archive/dplyr/
      }

    }
    if(.repo=="nexus"){
      if(!.overview){
        return(file.path(.cran.mirror, paste0(.main.path, paste(R.version$major, R.version$minor, sep="."), "/"), .package, .version, paste0(.package,"_",.version,".zip"))) # e.g. https://<***>.de/service/rest/repository/browse/r-public/bin/windows/contrib/4.2/dbplyr/2.3.1/dbplyr_2.3.1.zip
      } else {
        return(file.path(.cran.mirror, .archiv.path, paste(R.version$major, R.version$minor, sep="."), .package)) # e.g. https://<***>.de/service/rest/repository/browse/r-public/bin/windows/contrib/4.2/dbplyr/
      }
    }
}
