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
