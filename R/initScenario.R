##--#####################################################--##
#### Transform the input table in an optimLanduse object ####
##--#####################################################--##

# Fri Feb  5 10:40:10 2021 ------------------------------


#' Initialize the robust optimization
#'
#' The function initializes an \emph{optimLanduse} S3 object on the
#' basis of a coefficients table. Please note that the coefficients table must follow
#' the \emph{optimLanduse} format. The expected format is explained in the example on
#'  \href{https://gitlab.gwdg.de/forest_economics_goettingen/optimlanduse}{GitLab}.
#'  Usage of \code{\link{dataPreparation}} is recommended to ensure that
#'  the format requirements are met.
#'
#'  The aim of separating the initialization from the optimization is to save
#'  computation time in batch analysis. The separated function calls allow the
#'  user to perform multiple
#'  optimizations from one initialized object. This could save time in scenario or
#'  sensitivity analysis.
#'
#' @param coefTable Coefficient table in the expected \emph{optimLanduse} format.
#' @param uValue \emph{u} Value. The uncertainty (standard deviation or standard error) is
#' multiplied with the u value. The value therefore enables scenario analyses with differing
#' uncertainties in relation to indicator values. Higher u values can be interpreted as a higher
#' risk aversion of the decision maker.
#' @param optimisticRule Either \emph{expectation} or \emph{uncertaintyAdjustedExpectation}.
#' The rule indicates whether the optimistic outcomes of an indicator are directly
#' reflected by their expectations or if the indicator is calculated as expectation +
#' uncertainty when "more is better", expectation - uncertainty respectively when "less is better".
#' An optimization based on \emph{expectation} considers only downside risks.
#' @param fixDistance A two-column table or matrix. The table must
#' contain the best and the worst performing landuse-type of every uncertainty
#' scenario, which is influenced by the \emph{uValue}. The difference between
#' these two variables reflects the uncertainty space, in other words the
#' distance. This table can always be found (no matter if the distance is fixed
#' or not) in result list of the \emph{initScenario} function. By default, the
#' distance is not fixed \emph{NULL}. Fixing the distance allows you to change
#' the uncertainty level, without changing the uncertainty framework. For
#' instance, you can then relate the achieved portfolio performance, with a low
#' uncertainty level, to a wider and constant uncertainty framework within your
#' analysis; so the \emph{betas} remain comparable with each other over the
#' course of the uncertainty analysis.
#' @return An initialized optimLanduse S3 object ready for optimization.
#' @examples
#' require(readxl)
#' dat <- read_xlsx(exampleData("exampleGosling_2020.xlsx"),
#'                  col_names = FALSE)
#' dat <- dataPreparation(dat, uncertainty = "SE", expVAL = "score")
#' init <- initScenario(dat,
#'                      uValue = 2,
#'                      optimisticRule = "expectation",
#'                      fixDistance = NULL)

#' @import dplyr
#' @import tidyr
#' @importFrom stats setNames
#'
#' @export
initScenario <- function(coefTable,  uValue = 1, optimisticRule = "expectation", fixDistance = NULL) {

  #-----------------------------------------#
  #### Check the format of the coefTable ####
  #-----------------------------------------#

  if (!all(c("indicator", "direction", "landUse", "indicatorValue", "indicatorUncertainty") %in% names(coefTable))) {
    stop ("At least one necessary variable for the optimization is not available. Are the requirements of the data structure met? Check the variable names.")
  }

  indicatorNames <- as.character(unique(coefTable$indicator))

  testLandUseIndicators <- function (x) {
    all(indicatorNames %in% x)
  }

  checkLanduseTemp <- stats::aggregate(indicator ~ landUse, FUN = testLandUseIndicators, data = coefTable)


  if (!all(checkLanduseTemp$indicator)) {
    stop ("At least one indicator is not available for at least one land-use option.")
  }
  if (!length(indicatorNames) * length(unique(coefTable$landUse)) == nrow(coefTable)) {
    stop ("The indicator names are not unique. Have you assigned an indicator name twice?")
  }

  #----------------------------#
  #### Initialise the table ####
  #----------------------------#

  landUse <- as.character(unique(coefTable$landUse))

  expandList <- list()
  expandList[landUse] <- list(c("High", "Low"))

  expandMatrix1 <- as.matrix(expand.grid(expandList, stringsAsFactors = FALSE))
  expandMatrix2 <- do.call(rbind, replicate(length(indicatorNames), expandMatrix1, simplify = FALSE))
  scenarioTable <- tibble(indicator = rep(indicatorNames, each = dim(expandMatrix1)[1])) %>%
    bind_cols(as_tibble(expandMatrix2))

  names(scenarioTable)[names(scenarioTable) %in% landUse] <- paste0("outcome",names(scenarioTable)[names(scenarioTable) %in% landUse])

  #--------------------#
  ## Attach direction ##
  #--------------------#

  scenarioTableTemp1 <- scenarioTable
  scenarioTable <- merge(scenarioTable, unique(coefTable[, c("indicator", "direction")]), by = "indicator")
  if(!dim(scenarioTableTemp1)[1] == dim(scenarioTable)[1]) {cat("Error: Direction mising or wrong.")}

  #---------------------------------------------#
  ## Attach indicator values and uncertainties ##
  #---------------------------------------------#

  scenarioTableTemp2 <- scenarioTable

  spread1 <- tidyr::spread(data = coefTable[, !names(coefTable) == "indicatorUncertainty"],
                    key = landUse,
                    value = "indicatorValue")
  names(spread1)[names(spread1) %in% eval(landUse)] <- paste0("mean", names(spread1)[names(spread1) %in% eval(landUse)])


  spread2 <- tidyr::spread(data = coefTable[, !names(coefTable) == "indicatorValue"],
                     key = landUse,
                     value = "indicatorUncertainty")
  names(spread2)[names(spread2) %in% eval(landUse)] <- paste0("sem", names(spread2)[names(spread2) %in% eval(landUse)])


  for(i in landUse) {
    byIndicator <- c("indicator")
    names(byIndicator) <- "indicator"
    scenarioTable <- left_join(scenarioTable, spread1[, c("indicator", paste0("mean", i))], by = byIndicator)
    scenarioTable <- left_join(scenarioTable, spread2[, c("indicator", paste0("sem", i))], by = byIndicator)
  }


  scenarioTable <- scenarioTable %>% select(-contains("sem"), everything()) # Alternatively, but slower, a second loop would be suitable

  if(!dim(scenarioTableTemp1)[1] == dim(scenarioTable)[1]) {cat("Error: Attaching expectation or uncertainty failed.")}

  #--------------------------------------------#
  ## Calculate indicator uncertainty adjusted ##
  #--------------------------------------------#

  scenarioTableTemp3 <- scenarioTable


  newColumnNames <- paste0("adjSem", landUse)
  scenarioTable[, newColumnNames] <- NA # Initialise empty

  for(i in landUse) {
    # Ugly. But fast and less error-prone
    scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
      scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
      scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * uValue

    scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
      scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
      scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * uValue

    if(optimisticRule == "uncertaintyAdjustedExpectation") {
      scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
        scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
        scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * uValue

      scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
        scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
        scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * uValue
    }
    if(optimisticRule == "expectation") {
      scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
        scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

      scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
        scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]
    }
  }

  if(!optimisticRule %in% c("uncertaintyAdjustedExpectation", "expectation")) {cat("optimisticRule must be uncertaintyAdjustedExpectation or expectation")}
  if(!dim(scenarioTableTemp3)[1] == dim(scenarioTable)[1] | any(is.na(scenarioTable))) {cat("Error: Calculation of adjusted uncertainty.")}

  #--------------------------#
  ## calculate Min Max Diff ##
  #--------------------------#
  if(is.null(fixDistance)){
    scenarioTable[, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
      apply(scenarioTable[, startsWith(names(scenarioTable), "adjSem")], 1,
            function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()
  } else if (dim(fixDistance)[1] == dim(scenarioTable)[1] &&
             length(fixDistance)==2) {
    scenarioTable[, c("minAdjSem", "maxAdjSem")] <- fixDistance
    scenarioTable$diffAdjSem <- scenarioTable$maxAdjSem - scenarioTable$minAdjSem
  } else {stop(paste("The dimension of the 'fixDistance' (min and max) must contain: 2 columns and",
                     dim(scenarioTable)[1], "rows."))}


  #-------------------------------------------------------------#
  ## Define the coefficients for the linear objective function ##
  #-------------------------------------------------------------#

  #and the restrictions. (Simplify the scenario to a row problem)
  coefObjective <- defineObjectiveCoefficients(scenarioTable)

  #-------------------------------------#
  #### Define the constraints matrix ####
  #-------------------------------------#

  constraintCoefficients <- defineConstraintCoefficients(scenarioTable)

  retList <- list(scenarioSettings = data.frame(uValue = uValue,
                              optimisticRule = optimisticRule, stringsAsFactors = FALSE),
                  scenarioTable = scenarioTable,
                  coefObjective = coefObjective,
                  coefConstraint = constraintCoefficients,
                  distance = scenarioTable[, c("minAdjSem", "maxAdjSem")],
                  status = "initialized",
                  beta = NA,
                  landUse = setNames(data.frame(matrix(rep(NA, length(landUse)), ncol = length(landUse), nrow = 1)), landUse),
                  optimDetails = list()
)
  class(retList) <- "optimLanduse"
  return(retList)
}


