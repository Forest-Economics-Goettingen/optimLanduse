##--#####################################################--##
#### Transform the input table in an optimLanduse object ####
##--#####################################################--##

# Fri Jan 24 23:53:50 2020 ------------------------------

#' Initialize the robust optimization
#'
#' The function translates the prepared lanUse object into a solvable *optimLanduse* S3 object. Aim of the separation of the initialization and the optimization is to save time in the (possibly time-cunsuming) optimization.
#'
#' @param coefTable Parameter and uncertainties in a specific format. Usage of the 'dataPreparation' function is recommended to make sure, the format requiremnts are met.
#' @param uValue u Value.
#' @param optimisticRule Either *expectation* or *uncertaintyAdjustedExpectation*. It indicates whether the optimistic outcomes of an indicator are directly reflected by the *expectation* or if the indicator is calculated *adjusted* by *expectation* + *uncertainty*.
#' @param fixDistance tbd.
#' @return An initialized landUse object ready for optimization.

#' @import dplyr
#' @import tidyr
#' @importFrom stats setNames
#'
#' @export
initScenario <- function(coefTable,  uValue = 3, optimisticRule = "expectation", fixDistance = NULL) {

  #-----------------------------------------#
  #### Check the format of the coefTable ####
  #-----------------------------------------#

  if (!all(c("indicator", "direction", "landUse", "indicatorValue", "indicatorUncertainty") %in% names(coefTable))) {
    stop ("At least one necessary variable for the optimization is not available. Are the requirements of the data structure met? Check the variable names.")
  }

  indicatorNames <- as.character(unique(coefTable$indicator))

  # all(indicatorNamesCheck %in% coefTable$indicator[coefTable$landUse == "Forest"]) # useless
  testLandUseIndicators <- function (x) {
    all(indicatorNames %in% x)
  }

  if (!coefTable %>% group_by(landUse) %>% summarise(checkLanduse = testLandUseIndicators(indicator)) %>% pull(checkLanduse) %>% all()) {
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
  # scenarioTable <- tibble(indicator = rep(indicatorNames, each = dim(expandMatrix1)[1])) %>%
  #   left_join(indicatorNames, by = "indicator") %>% bind_cols(as_tibble(expandMatrix2))
  # Alter Version. Evtl relevant bei Fehlersuche. Ich weiß nicht mehr was ich mir bei dem left join gedacht habe.
  # tbd. Tidy raus
  # scenarioTable <- scenarioTable %>% rename_at(.vars = vars(!!landUse[1] : !!landUse[length(landUse)]),
  #                                              .funs = funs(paste0("outcome", .))) #.funs deprecated

  names(scenarioTable)[names(scenarioTable) %in% landUse] <- paste0("outcome",names(scenarioTable)[names(scenarioTable) %in% landUse])

  #--------------------#
  ## Attach direction ##
  #--------------------#

  scenarioTableTemp1 <- scenarioTable
  scenarioTable <- merge(scenarioTable, unique(coefTable[, c("indicator","direction")]), by = "indicator")
  if(!dim(scenarioTableTemp1)[1] == dim(scenarioTable)[1]) {cat("Error: Direction mising or wrong.")}

  #---------------------------------------------#
  ## Attach indicator values and uncertainties ##
  #---------------------------------------------#

  scenarioTableTemp2 <- scenarioTable

  # Das muss noch umgeschrieben werden, da funs "deprecated" ist: Am besten ganz ohne tidy ...

  spread1 <- coefTable %>% select(-indicatorUncertainty) %>% spread(key = landUse, value = indicatorValue)
  names(spread1)[names(spread1) %in% eval(landUse)] <- paste0("mean", names(spread1)[names(spread1) %in% eval(landUse)])

  spread2 <- coefTable %>% select(-indicatorValue) %>% spread(key = landUse, value = indicatorUncertainty)
  names(spread2)[names(spread2) %in% eval(landUse)] <- paste0("sem", names(spread2)[names(spread2) %in% eval(landUse)])

  for(i in landUse) {
    byIndicator <- c("indicator")
    names(byIndicator) <- "indicator"
    scenarioTable <- left_join(scenarioTable, spread1[, c("indicator", paste0("mean", i))], by = byIndicator)
    scenarioTable <- left_join(scenarioTable, spread2[, c("indicator", paste0("sem", i))], by = byIndicator)
  }

  # scenarioTable <- scenarioTable %>% select(-contains("mean"), everything()) # Order the variables, such that the means and uncertainties follow in direct succession
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
    # scenarioTable[, c("minAdjSem", "maxAdjSem")] <-
    # apply(scenarioTable[, startsWith(names(scenarioTable), "adjSem")], 1,
    #       function(x) {c(min(x), max(x))}) %>% t()
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


