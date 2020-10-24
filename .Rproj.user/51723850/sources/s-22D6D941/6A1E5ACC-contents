##--#####################################################--##
#### Transform the input table in an optimLanduse object ####
##--#####################################################--##

# Fri Jan 24 23:53:50 2020 ------------------------------

#' Initialize the robust optimization
#'
#' The function translates the indicators values and uncertainties for the land-use options into a solvable *optimLanduse* object.
#'
#' @param coefTable See the exemplary import folder.
#' @param uValue u Value.
#' @param optimisticRule Either *expectation* or *uncertaintyAdjustedExpectation*. It indicates weather the optimistic outcomes of an indicator are directly reflected by the *expectation* or if the indicator is *adjusted*.
#' @return An initialized landUse portfolio ready for optimization.

#' @import dplyr
#' @import tidyr

#' @export
initScenario <- function(coefTable,  uValue = 3, optimisticRule = "expectation") {

  #----------------------------#
  #### Initialise the table ####
  #----------------------------#

  landUse <- as.character(unique(coefTable$landUse))
  indicatorNames <- as.character(unique(coefTable$indicator))

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
  scenarioTable <- scenarioTable %>% rename_at(.vars = vars(!!landUse[1] : !!landUse[length(landUse)]), .funs = funs(paste0("outcome", .)))

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

  scenarioTable <- scenarioTable %>% select(-contains("mean"), everything()) # Order the variables, such that the means and uncertainties follow in direct succession
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

  scenarioTable[, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
    apply(scenarioTable[, startsWith(names(scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()

  #-------------------------------------------------------------#
  ## Define the coefficients for the linear objective function ##
  #-------------------------------------------------------------#

  #and the restrictions.
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
                  status = "initialized",
                  beta = NA,
                  landUse = setNames(data.frame(matrix(rep(NA, length(landUse)), ncol = length(landUse), nrow = 1)), landUse),
                  optimDetails = list()
)
  class(retList) <- "optimLanduse"
  return(retList)
}

