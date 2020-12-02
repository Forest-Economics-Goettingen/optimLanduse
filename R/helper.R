#----------------------------------------------------------#
#### Define the coefficients for the objective function ####
#----------------------------------------------------------#
# Calculate coefficients, one for each variable from the scenario table

#' @export
defineConstraintCoefficients <- function (scenarioTable) {
  tempTableMore <- scenarioTable %>% filter(direction == "more is better") %>%
  mutate(across(starts_with("adjSem"),
                ~{(. - minAdjSem) / diffAdjSem},
                .names = "{.col}_modified"))

  tempTableLess <- scenarioTable %>% filter(direction == "less is better") %>%
  mutate(across(starts_with("adjSem"),
                ~{(maxAdjSem - .) / diffAdjSem},
                .names = "{.col}_modified"))

  tempTableMore %>% bind_rows(tempTableLess) %>% select(ends_with("modified")) %>% as.matrix() %>%
    return()
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
