##--#####################################################--##
#### Attach portfolio performance and distance to target ####
##--#####################################################--##
# Wed Jan 29 16:19:22 2020 ------------------------------

# Main developer: Kai Bödeker, Kai Husmann

#' Attach portfolio performance and distance to target
#'
#' The Portfolio performances is calculated and attached to the solved
#' optimLanduse object. Each performance measure describes the relative proportion to the maximum achievable
#' (the "target") of the indicator, given the current solution. The lowest performance of all indicators
#' is the degree of fulfillment of the worst performing
#' indicator. It can thus be intepreted as the guaranteed performance
#' under the worst-possible scenario. At least this proportion will be
#' achieved across all indicators.
#'
#' For further information and calculation see the supplement of Gosling et al (2020),
#' Formula S5 (in the supplement of the paper) and also the paragraph
#' optimLanduse functions and workflow - Post-processing in Husmann et al (n. d.).
#'
#' @param x An optimized optimLanduse object.
#' @return An optimized optimLanduse object with attached portfolio performance.
#' @references Gosling, E., Reith, E., Knoke T., Gerique, A., Paul, C. (2020): Exploring
#' farmer perceptions of agroforestry via multi-objective optimisation: a test application
#' in Eastern Panama. \emph{Agroforestry Systems} \strong{94}. \doi{10.1007/s10457-020-00519-0}
#'
#' Husmann, K., von Groß, V., Bödeker, K., Fuchs, J. M., Paul, C., Knoke, T. (no date): optimLanduse:
#' A Package for Multiobjective Land-cover1Composition Optimization under Uncertainty. \emph{Methods
#' in Ecology and Management}. Under review.
#'
#' @examples
#' require(ggplot2)
#' require(readxl)
#'
#' dat <- read_xlsx(exampleData("exampleGosling.xlsx"))
#' init <- initScenario(dat, uValue = 2,
#'                      optimisticRule = "expectation",
#'                      fixDistance = 3)
#' result <- solveScenario(x = init)
#' performance <- calcPerformance(result)
#'
#' # Visualize the distance
#'
#' ggplot(performance$scenarioTable,
#'        aes(x = indicator,
#'            y = performance,
#'            color = indicator)) +
#' geom_point() +
#' geom_hline(yintercept =
#'            min(performance$scenarioTable$performance),
#'           linetype = "dashed", color = "red") +
#' ylim(0, 1)

#' @importFrom utils type.convert

#' @export
calcPerformance <- function(x) {
  # tbd. Umschreiben: den Scenario Table einmal am Anfang in eine Variable schreiben, um das x$ zu vermeiden.
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
  # x$scenarioTable <- x$scenarioTable %>%
  #   mutate(distanceToTargetPerformance = 1 - ifelse(direction == "more is better",
  #                                                   ((portfolioPerformance - minAdjSem) / diffAdjSem),
  #                                                   ((maxAdjSem - portfolioPerformance) / diffAdjSem)))

  x$scenarioTable[x$scenarioTable$direction == "less is better", "performance"] <-
    1 - (x$scenarioTable$portfolioPerformance[x$scenarioTable$direction == "less is better"] -
           x$scenarioTable$minAdjSem[x$scenarioTable$direction == "less is better"]) /
    x$scenarioTable$diffAdjSem[x$scenarioTable$direction == "less is better"]

  x$scenarioTable[x$scenarioTable$direction == "more is better", "performance"] <-
    1 - (x$scenarioTable$maxAdjSem[x$scenarioTable$direction == "more is better"] -
           x$scenarioTable$portfolioPerformance[x$scenarioTable$direction == "more is better"]) /
    x$scenarioTable$diffAdjSem[x$scenarioTable$direction == "more is better"]

  if(any(is.na(x$scenarioTable$performance))) {cat("Warning: Not all performances calculated.")}

  x$status <- "optimized - information updated" # function will show an Error if it is run twice

  return(x)

}
