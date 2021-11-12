##--################--##
#### sumSensitivity ####
##--################--##
# Maintainer: Leona Ottens
# Developer: Leona Ottens, Volker von Groß

#' Give out the sensitivity analysis as an Excel file.
#'
#' The function gives out the results of the sensitivity analysis as an excel file. It provides the possibility to
#' select specific landUse options and indicators for which the results of the sensitivity analysis are exported into Excel.
#'
#' @param x Output of the \code{\link{calcSensitivity}} function.
#' @param landUse Vector with the landUse options for which the results of the sensitivity analysis are exported into an Excel file.
#' By default, all landUse options are selected.
#' @param indicator Vector with the indicators for which the results of the sensitivity analysis are exported into an Excel file.
#' By default, all indicators are selected.
#' @param fileName A character string with the file name.
#' @return An Excel file containing the output of the \code{\link{calcSensitivity}} function for the chosen landUse options and indicators.
#' @examples
#' require(readxl)
#' dat <- read_xlsx(exampleData("exampleGosling_2020.xlsx"),
#'                  col_names = FALSE)
#' dat <- dataPreparation(dat, uncertainty = "SE", expVAL = "score")
#' init <- initScenario(dat,
#'                      uValue = 2,
#'                      optimisticRule = "expectation",
#'                      fixDistance = NULL)
#' result <- solveScenario(x = init)
#' sensitivity <- calcSensitivity(x = result, y = 0.01, digits = 3, fixDistance = NULL)
#' sumSensitivity(x = sensitivity,
#'                landUse = c("Crops", "Pasture", "Forest"),
#'                indicator = c("Long-term income", "Liquidity", "Protecting water supply"),
#'                fileName = "sensitivity_analysis_exampleData")
#'
#' @import dplyr
#' @import xlsx

#' @export
 sumSensitivity <- function(x, landUse = colnames(x$landUse), indicator = unique(x$scenarioTable$indicator), fileName = "sensitivity_analysis"){

  x$AllowableIncreaseValue[x$AllowableIncreaseValue == Inf] <- .Machine$double.xmax
  x$AllowableIncreaseUncertainty[x$AllowableIncreaseUncertainty == Inf] <- .Machine$double.xmax
  x$AllowableDecreaseValue[x$AllowableDecreaseValue == Inf] <- .Machine$double.xmax
  x$AllowableDecreaseUncertainty[x$AllowableDecreaseUncertainty == Inf] <- .Machine$double.xmax

  write.xlsx(x$AllowableIncreaseValue[x$AllowableIncreaseValue$landUse %in% landUse & x$AllowableIncreaseValue$indicator %in% indicator, ], file = paste0(fileName, ".xlsx"), sheetName = "AllowableIncreaseValue", row.names = FALSE)
  write.xlsx(x$AllowableIncreaseUncertainty[x$AllowableIncreaseUncertainty$landUse %in% landUse & x$AllowableIncreaseUncertainty$indicator %in% indicator, ], file = paste0(fileName, ".xlsx"), sheetName = "AllowableIncreaseUncertainty", append = TRUE, row.names = FALSE)
  write.xlsx(x$AllowableDecreaseValue[x$AllowableDecreaseValue$landUse %in% landUse & x$AllowableDecreaseValue$indicator %in% indicator, ], file = paste0(fileName, ".xlsx"), sheetName = "AllowableDecreaseValue", append = TRUE, row.names = FALSE)
  write.xlsx(x$AllowableDecreaseUncertainty[x$AllowableDecreaseUncertainty$landUse %in% landUse & x$AllowableDecreaseUncertainty$indicator %in% indicator, ], file = paste0(fileName, ".xlsx"), sheetName = "AllowableDecreaseUncertainty", append = TRUE, row.names = FALSE)
  write.xlsx(x$Increase[x$Increase$landUse %in% landUse & x$Increase$indicator %in% indicator, ], file = paste0(fileName, ".xlsx"), sheetName = "Increase", append = TRUE, row.names = FALSE)
  write.xlsx(x$Decrease[x$Decrease$landUse %in% landUse & x$Decrease$indicator %in% indicator, ], file = paste0(fileName, ".xlsx"), sheetName = "Decrease", append = TRUE, row.names = FALSE)

}
