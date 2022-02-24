#----------------------------------------------------------#
#### Define the coefficients for the objective function ####
#----------------------------------------------------------#
# Calculate coefficients, one for each variable from the scenario table

#' Internal function for the re-formulation of the long represantation of the coefficients of the robust optimization into nLanduse Parameters. Advantage is that the shorter representation corresponds the expected form of the objective function of the lpSolveAPI package.
#'
#'
#' @param scenarioTable The fully initialized scenario table.
#' @return The coefficients of the objective function.

#' @noRd
defineObjectiveCoefficients <- function(scenarioTable) {
  # Set "less is better to negative" and divide by maximum difference
  scenarioTable[scenarioTable$direction == "less is better", grep(c("^adjSem"), names(scenarioTable))] <-
    -scenarioTable[scenarioTable$direction == "less is better", grep(c("^adjSem"), names(scenarioTable))]

  scenarioTable[, grep(c("^adjSem"), names(scenarioTable))] <-
    scenarioTable[, grep(c("^adjSem"), names(scenarioTable))] / scenarioTable$diffAdjSem * 100

  coefObjective <- apply(scenarioTable[, grep(c("^adjSem"), names(scenarioTable))], 2, sum)

  return(coefObjective)
}

#-------------------------------------#
#### Define the constraints matrix ####
#-------------------------------------#

#' Internal function for the re-formulation of the long represantation of the constraints of the robust optimization into nLanduse colums (but still the original row length). Advantage is that the representation corresponds the expected form of the constraints of the lpSolveAPI package.
#'
#'
#' @param scenarioTable The fully initialized scenario table.
#' @return The coefficients of the objective function.

#' @noRd
defineConstraintCoefficients <- function (scenarioTable) {

  tempTableMore <- scenarioTable[scenarioTable$direction == "more is better", ]
  tempTableMore <- tempTableMore %>%
    mutate(across(starts_with("adjSem"),
                  ~{(. - minAdjSem) / diffAdjSem},
                  .names = "{.col}_modified"))

  tempTableLess <- scenarioTable[scenarioTable$direction == "less is better", ]
  tempTableLess <- tempTableLess %>%
    mutate(across(starts_with("adjSem"),
                  ~{(maxAdjSem - .) / diffAdjSem},
                  .names = "{.col}_modified"))

  tempTableMore %>% bind_rows(tempTableLess) %>% select(ends_with("modified")) %>% as.matrix() %>%
    return()
}

#--------------------------------------------#
## Calculate indicator uncertainty adjusted ##
#--------------------------------------------#

#' Internal function for the re-formulation of the long represantation of the constraints of the robust optimization into nLanduse colums (but still the original row length). Advantage is that the representation corresponds the expected form of the constraints of the lpSolveAPI package.
#'
#'
#' @param scenarioTable The fully initialized scenario table.
#' @param landUse The land use options
#' @param uValue The uValue
#' @return The updated scenario table

#' @noRd
addAdjSEM <- function (scenarioTable = scenarioTable,
                       landUse = landUse,
                       uValue = uValue,
                       optimisticRule = optimisticRule
                       ) {

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

  return(scenarioTable)
}
