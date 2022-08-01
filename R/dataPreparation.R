##--##################--##
#### Data preparation ####
##--##################--##

# Tue Jul  5 16:48:59 2022 ------------------------------

# Main developer: Kai Husmann, Kai Bödeker, Volker von Groß

#' Transform data into the expected format
#'
#' The input data must suit the specific expected optimLanduse format prior to
#' initialization and optimization. This function
#' provides the possibility to easily transform data from the commonly used form
#' of the exemplary data
#' \code{\link{exampleData}} into the expected format. Please consider that the application of this function
#' is not mandatory and in most cases not required. Best practice is to
#' transform your data yourself into the expected format. Detailed information
#' about the expected format and possible data processing can be found on the
#' \href{https://github.com/Forest-Economics-Goettingen/optimLanduse/}{GitHub project page}.
#' Note that incomplete rows, which include NA-values will be deleted
#' and an error message will be thrown.
#'
#' @param dat Data frame or tibble in the format of the \code{\link{exampleData}}.
#' Please refer to the \href{https://github.com/Forest-Economics-Goettingen/optimLanduse/}{GitHub project page}
#' for more details.
#' @param uncertainty Indicates the column name of the uncertainty measure.
#' Typical is "SE" for standard
#' error or "SD" for standard deviation.
#' @param expVAL Indicates the column name of the expected value.
#' @return A formatted coefficients table with land-use options and indicator values ready for initialization via \code{\link{initScenario}}.
#' @references Gosling, E., Reith, E., Knoke, T. et al. Exploring farmer perceptions of agroforestry via multi-objective optimisation:
#' a test application in Eastern Panama. Agroforest Syst 94, 2003–2020 (2020). https://doi.org/10.1007/s10457-020-00519-0
#' @examples
#' require(readxl)
#' dat <- read_xlsx(exampleData("exampleGosling_dataPrep.xlsx"), col_names = TRUE)
#' dat <- dataPreparation(dat, uncertainty = "sd", expVAL = "mean")

#' @import dplyr
#' @importFrom stats na.omit
#' @importFrom utils type.convert

#' @export
dataPreparation <- function(dat, uncertainty = "SE", expVAL = "mean"){

    ## Convert input Data to dat.final ##
    ## Filter all Rows with only NA ##
    dat.final <- dat[rowSums(is.na(dat)) != ncol(dat), ]
    dat.final <- dat.final[colSums(!is.na(dat.final)) > 2] # columns filled with NAs will otherwise be deleted <- can be fatal if e.g., column "branch" left empty
    if(any(is.na(dat.final[, 1]))){dat.final <- dat.final[-1, ]}

    ## Create column names ##
    colnames(dat.final) <- dat.final[1, ]
    dat.final <- dat.final[-1, ]

    ## rename duplicated Columnnames ##
    names(dat.final) <- make.unique(colnames(dat.final))

    ## detect and set classes of a dat.final
    dat.final <- lapply(dat.final, type.convert) %>% bind_cols()

    ## rename first columns for initScenario function and define data structure ##
    chtr.cols <- unlist(lapply(dat.final[1,],is.numeric))
    chtr.cols <- length(chtr.cols[chtr.cols == FALSE])
    dat.final[, (chtr.cols+1) : ncol(dat.final)][is.na(dat.final[, (chtr.cols + 1) : ncol(dat.final)])] <- 0

    ## warn and delete factor rows with NA ##
    if(any(is.na(dat.final[, 1:chtr.cols]))){warning("Some Indicators have missing value, rows got deleted")}
    # which(is.na(dat.final[, 1:chtr.cols]), arr.ind = TRUE)
    dat.final <- na.omit(dat.final)

    ## select landUse names ##
    landUse <- names(dat)
    landUse <- landUse[!grepl("\\.\\.", landUse)]

    ## select mean values, rename columns and gather ##
    importValues <- dat.final %>% select((1:all_of(chtr.cols)), starts_with(expVAL))
    colnames(importValues)[grepl(expVAL, colnames(importValues))] <- landUse
    importValues <- importValues %>%  gather(key = "landUse", value = "indicatorValue", landUse[1]:landUse[length(landUse)])

    ## select uncertainty, rename columns and gather ##
    importUnc <- dat.final %>% select((1:all_of(chtr.cols)), starts_with(uncertainty))
    colnames(importUnc)[grepl(uncertainty, colnames(importUnc))] <- landUse
    importUnc <- importUnc %>%  gather(key = "landUse", value = "indicatorUncertainty", landUse[1]:landUse[length(landUse)])

    ## combine mean and uncertainty ##
    dataSource <- left_join(importValues, importUnc, by = c(names(dat.final)[1:chtr.cols], "landUse"))
  return(dataSource)
}

