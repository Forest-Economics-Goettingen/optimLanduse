##--#####################################################--##
#### Attach portfolio performance and distance to target ####
##--#####################################################--##
# Wed Jan 29 16:19:22 2020 ------------------------------
# Kai Husmann

#' Attach portfolio performance and distance to target
#'
#' The function calculates and attaches the portfolio performance and distance to target. See Gosling et al. Equations 10 and 11.
#' @param x An optimized optimLanduse object.

#'@export
calcDistanceToPerformanceScenario <- function(x) {

  if(!all(names(x$scenarioTable[, startsWith(names(x$scenarioTable), "adj")]) ==
          paste0("adjSem", names(x$landUse)))) {
    cat("Error: Unexpected variables in the scenario table.")
  }

  if(!x$status == "optimized") {cat("Error: No optimim found. Did you call solveScenario?")}
  #---------------------------------#
  #### Add portfolio performance ####
  #---------------------------------#

  # See e.g. Gosling et al. Eq. 10

  #rep(averageNomimalIndicatorValue[1 ,], each = dim(scenarioTable)[1])

  x$scenarioTable$portfolioPerformance <- apply(do.call(rbind, replicate(dim(x$scenarioTable)[1],
                                                                         x$landUse[1 ,], simplify = FALSE)) *
                                                  x$scenarioTable[, startsWith(names(x$scenarioTable), "adj")], 1, sum)

  #------------------------------------------#
  #### Add distance to target performance ####
  #------------------------------------------#

  # See. e.g. Gosling et al. Eq. 11
  x$scenarioTable <- x$scenarioTable %>% mutate(distanceToTargetPerformance = 1 - ifelse(direction == "more is better",
                                                                                           ((portfolioPerformance - minAdjSem) / diffAdjSem),
                                                                                           ((maxAdjSem - portfolioPerformance) / diffAdjSem)))

  x$status <- "optimized - information updated"

  return(x)

}
