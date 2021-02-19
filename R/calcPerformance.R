##--#####################################################--##
#### Attach portfolio performance and distance to target ####
##--#####################################################--##
# Wed Jan 29 16:19:22 2020 ------------------------------
# Maintainer: Kai Husmann
# Developer: Kai Bödeker, Kai Husmann

#' Attach portfolio performance and distance to target
#'
#' The function calculates and attaches the portfolio performance.
#' For a comprehensive interpretation the beta values have to be grouped by
#' their indicator in a plot  (see example).
#' Each beta describes the relative proportion to the maximum achievable
#' (the "target") within its indicator, given the current land use distribution
#' and the uncertainty scenario set. The lowest beta of all indicators
#' guarantees that under a worst-case scenario, at least this proportion will be
#' achieved across all indicators. The solveScenario() function maximizes the
#' guaranteed performance, or minimizes the distance to the maximum possible.
#'
#' For further information see the supplement of Gosling et al (2020), Formula S5 (in the supplement of the paper).
#'
#' @param x An optimized optimLanduse object.
#' @return An optimized optimLanduse object with attached portfolio performance.
#' @references Gosling, E., Reith, E., Knoke T., Gerique, A., Paul, C. (2020): Exploring
#' farmer perceptions of agroforestry via multi-objective optimisation: a test application
#' in Eastern Panama. \emph{Agroforestry Systems} \strong{94}. \url{https://doi.org/10.1007/s10457-020-00519-0}
#' @examples
#' require(ggplot2)
#' require(readxl)
#'
#' dat <- read_xlsx(exampleData("exampleGosling_2020.xlsx"),
#'                  col_names = FALSE)
#' dat <- dataPreparation(dat, uncertainty = "SE", expVAL = "score")
#' init <- initScenario(dat, uValue = 2,
#'                      optimisticRule = "expectation",
#'                      fixDistance = NULL)
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
