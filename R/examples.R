##--#######################--##
#### Attach examplary data ####
##--#######################--##

# Tue Jul  5 17:18:58 2022 ------------------------------

# Main developer: Volker von Groß


#' optimLanduse comes bundled with exemplary data for land-use optimization. The
#' files can also be found on your computer in the package folder `./extdata`. These examples
#' provide some quick applications of the package for demonstration and an example of the expected structure
#' of the data. Consider also the
#' \href{https://github.com/Forest-Economics-Goettingen/optimLanduse/}{GitHub project page} for
#' exemplary applications of the package.
#'
#' \emph{exampleGosling.xlsx} is an excerpt from Gosling et al. (2020).
#'
#' @param fileName Name of example file. See 'details' section for further explanation of all provided examples.

#' @return The path to the example file on your computer.
#'
#' @examples
#' require(readxl)
#' path <- exampleData()
#' read_xlsx(path, col_names = FALSE)
#' path <- exampleData("exampleGosling.xlsx")
#' read_xlsx(path, col_names = FALSE)
#'
#' @references Gosling, E., Reith, E., Knoke, T. et al. Exploring farmer perceptions of agroforestry via multi-objective optimisation:
#' a test application in Eastern Panama. Agroforest Syst 94, 2003–2020 (2020). https://doi.org/10.1007/s10457-020-00519-0

#' @export
exampleData <- function(fileName = "exampleGosling.xlsx") {
  possibleFiles <- dir(system.file("extdata", package = "optimLanduse"))
  if(fileName %in% possibleFiles) {
    system.file("extdata", fileName, package = "optimLanduse", mustWork = FALSE)
  } else {
    warning(paste0(c("Example not found. Possible file names are ", paste(possibleFiles, collapse = ", "), ".")))
  }
}
