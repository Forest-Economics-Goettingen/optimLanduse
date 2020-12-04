##--#####################################################--##
#### Attach portfolio performance and distance to target ####
##--#####################################################--##
# Wed Jan 29 16:19:22 2020 ------------------------------
# Maintainer: Kai Husmann
# Developer: Kai Husmann, Kai Bödecker

#' Attach portfolio performance and distance to target
#'
#' The function calculates and attaches the portfolio performance and distance to target. See Gosling, Reith, Knoke, Gerique and Paul (2020): Exploring farmer perceptions of agroforestry via multi-objective optimisation: a test application in Eastern Panama. Formula S5 (supplementary).
#' @param x An optimized optimLanduse object.
#' @return An optimized optimLanduse object with attached portfolio performance.

#' @importFrom utils type.convert

#'@export
calcDistanceToPerformanceScenario <- function(x) {
  # tbd. Umschreiben: den Scenario Table einmal am Anfang in eine Variable schreiben, um das x$ zu vermeiden. Diese 3 Stufen Indizierung wird vom rCheck angekreidet.
  # Oder noch besser: Ganz ohne dpyr

  if(!all(names(x$scenarioTable[, startsWith(names(x$scenarioTable), "adj")]) ==
          paste0("adjSem", names(x$landUse)))) {
    stop ("Error: Unexpected variables in the scenario table.")
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

  x$status <- "optimized - information updated" # function will show an Error if it is run twice

  return(x)

}
