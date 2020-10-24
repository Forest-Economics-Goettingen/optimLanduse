#----------------------------------------------------------#
#### Define the coefficients for the objective function ####
#----------------------------------------------------------#
# Calculate coefficients, one for each variable from the scenario table

#' @export
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

#' @export
defineConstraintCoefficients <- function (scenarioTable) {
  tempTableMore <- scenarioTable %>% filter(direction == "more is better") %>%
    mutate_at(vars(starts_with("adjSem")), funs(modified = (. - minAdjSem) / diffAdjSem))

  tempTableLess <- scenarioTable %>% filter(direction == "less is better") %>%
    mutate_at(vars(starts_with("adjSem")), funs(modified = (maxAdjSem - .) / diffAdjSem))

  tempTableMore %>% bind_rows(tempTableLess) %>% select(ends_with("modified")) %>% as.matrix() %>%
    return()
}
