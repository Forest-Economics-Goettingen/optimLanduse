##--#####################################################--##
#### Transform the input table in an optimLanduse object ####
##--#####################################################--##

# Tue Jul  5 17:18:58 2022 ------------------------------

# Main developer: Kai Husmann

#' Initialize the robust optimization
#'
#' The function initializes an \emph{optimLanduse} S3 object on the
#' basis of a coefficients table. Please note that the coefficients table must follow
#' the expected \emph{optimLanduse} format. The expected format is explained in the example on the
#' \href{https://github.com/Forest-Economics-Goettingen/optimLanduse/}{GitHub project page}.
#'
#'  The aim of separating the initialization from the optimization is to save
#'  computation time in batch analysis. The separated function calls allow the
#'  user to perform multiple
#'  optimizations from one initialized object. This could save time in scenario or
#'  sensitivity analysis.
#'
#'  A detailed description of the input parameters can be found in Husmann et al. (n.d.).
#'
#' @param coefTable Coefficient table in the expected \emph{optimLanduse} format.
#' @param uValue \emph{u} Value. The uncertainty value delivered in the coefTable is
#' multiplied with this u value. The value therefore enables scenario analyses with differing
#' uncertainties in relation to indicator values. Higher u values can be interpreted as a higher
#' risk aversion of the decision maker.
#' @param optimisticRule Either \emph{expectation} or \emph{uncertaintyAdjustedExpectation}.
#' The rule indicates whether the optimistic outcomes of an indicator are directly
#' reflected by their expectations or if the indicator is calculated as expectation +
#' uncertainty when "more is better", expectation - uncertainty respectively when "less is better".
#' An optimization based on \emph{expectation} considers only downside risks.
#' @param fixDistance This optional numeric value allows to define distinct uncertainty levels for the
#' calculation of the uncertainty space and the averaged distances of a certain land-cover composition
#' (see Equation 9 in Husmann et al. (n. d.)). Passing NA disables fixDistance. In this case,
#' the untertainty space is defined by uValue.
#' @return An initialized optimLanduse S3 object ready for optimization.
#' @references Husmann, K., von Groß, V., Bödeker, K., Fuchs, J. M., Paul, C., Knoke, T. (no date): optimLanduse:
#' A Package for Multiobjective Land-cover1Composition Optimization under Uncertainty. \emph{Methods
#' in Ecology and Management}. Under review.
#' @examples
#' require(readxl)
#' dat <- read_xlsx(exampleData("exampleGosling.xlsx"))
#'
#' init <- initScenario(dat,
#'                      uValue = 2,
#'                      optimisticRule = "expectation",
#'                      fixDistance = 3)

#' @import dplyr
#' @import tidyr
#' @importFrom stats setNames
#'
#' @export
initScenario <- function(coefTable,  uValue = 1, optimisticRule = "expectation", fixDistance = 3) {

  #-----------------------------------------#
  #### Check the format of the coefTable ####
  #-----------------------------------------#

  if (!all(c("indicator", "direction", "landUse", "indicatorValue", "indicatorUncertainty") %in% names(coefTable))) {
    stop ("At least one necessary variable for the optimization is not available. Are the requirements of the data structure met? Check the variable names.")
  }

  ## Drop unnecessary colums ##
  if(any(!(names(dat) %in%
           c("indicator", "direction", "landUse", "indicatorValue",
             "indicatorUncertainty", "indicatorGroup")))) {
    warning("Non-necessary columns detected and neglected.")
    coefTable <- coefTable[, names(coefTable) %in% c("indicator", "direction", "landUse", "indicatorValue",
                            "indicatorUncertainty", "indicatorGroup")]
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

  # add Adjusted SEM to the scenarioTable

  scenarioTable <- addAdjSEM(scenarioTable = scenarioTable, landUse = landUse,
                             uValue = uValue, optimisticRule = optimisticRule)


  if (!(fixDistance >=0 & fixDistance <= 10)  & !is.na(fixDistance)) {
    fixDistance <- NA
    warning("The fixDistance did not meet the requirements and therefore set to NA. Please find the possible values for the fixDistance in the help.")
  }

  if ((fixDistance >=0 & fixDistance <= 10)  & !is.na(fixDistance)) {
    scenarioTableFix <- addAdjSEM(scenarioTable = scenarioTableTemp3,
                                  landUse = landUse,
                                  uValue = fixDistance,
                                  optimisticRule = optimisticRule)
  }


  if(!optimisticRule %in% c("uncertaintyAdjustedExpectation", "expectation")) {cat("optimisticRule must be uncertaintyAdjustedExpectation or expectation")}
  if(!dim(scenarioTableTemp3)[1] == dim(scenarioTable)[1] | any(is.na(scenarioTable))) {cat("Error: Calculation of adjusted uncertainty.")}

  #--------------------------#
  ## calculate Min Max Diff ##
  #--------------------------#


  if (is.na(fixDistance)) {
    scenarioTable[, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
      apply(scenarioTable[, startsWith(names(scenarioTable), "adjSem")], 1,
            function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()
  } else {
    scenarioTable[, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
      apply(scenarioTableFix[, startsWith(names(scenarioTableFix), "adjSem")], 1,
            function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()

    # scenarioTableFix[, c("minAdjSem", "maxAdjSem")] <-
    #   apply(scenarioTable[, startsWith(names(scenarioTable), "adjSem")], 1,
    #       function(x) {c(min(x), max(x))}) %>% t()
    #scenarioTable <- scenarioTableFix
  } # Das könnte noch eleganter sein! tbd






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


