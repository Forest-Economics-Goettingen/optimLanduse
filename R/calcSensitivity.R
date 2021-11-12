
##--#################--##
#### calcSensitivity ####
##--#################--##
# Maintainer: Leona Ottens
# Developer: Leona Ottens, Kai Husmann, Volker von Groß

#' Perform a sensitivity analysis
#'
#' The function calculates the Allowable Increase and Decrease of the indicator value and uncertainty for each indicator and each land-use option. Furthermore, it calculates the
#' optimal land use allocation after the indicator value/uncertainty is increased/decreased by \emph{y} and compares it to the optimal land-use allocation of the optimization.
#'
#' @param x A solved optimLanduse S3 object. See \code{\link{solveScenario}} for the optimization.
#' @param y Decimal number, giving the percentage by which, the indicator value/uncertainty is increased/decreased.
#' @param digits Number of decimal places which are used in the comparison of the result of the iteration and the result of the optimization.
#' @param fixDistance See \code{\link{initScenario}} for further information.
#' @return An optimized optimLanduse object with attached sensitivity analysis. The sensitivity analysis gives out 4 data.frames with the results of the Allowable Increase and Decrease for the
#' indicator value and uncertainty. For the calculation of the Allowable Increase and Decrease the indicator values and uncertainties are increase/decrease up to 5000 %. In the case that the optimal land allocation still has not changed,
#' it will say \emph{Inf}.
#' Furthermore the sensitivity analysis gives out 2 data.frames each with the results of the increase/decrease of the indicators by \emph{y}. The data.frames contain the optimal land-use allocation
#' for each indicator-landuse-combination. If the optimal land allocation after the increase/decrease of the indicator value/uncertainty equals the original optimization result, the column \emph{sameResult} will say \emph{TRUE}, otherwise it
#' will say \emph{FALSE}.
#' @examples
#' require(readxl)
#' dat <- read_xlsx(exampleData("exampleGosling_2020.xlsx"),
#'                  col_names = FALSE)
#' dat <- dataPreparation(dat, uncertainty = "SE", expVAL = "score")
#' init <- initScenario(dat,
#'                      uValue = 2,
#'                      optimisticRule = "expectation",
#'                      fixDistance = NULL)
#' result <- solveScenario(x = init)
#' sensitivity <- calcSensitivity(x = result, y = 0.01, digits = 3, fixDistance = NULL)

#' @import dplyr

#' @export
calcSensitivity <- function(x, y = 0.01, digits = 3, fixDistance = NULL){


  #--------------------------------#
  #### Allowable Increase Value ####
  #--------------------------------#

  # Erzeugen eines leeren data.frames
  AllowableIncreaseValue <- data.frame(landUse = "",
                                       indicator = "",
                                       Allowable_Increase = "",
                                       Allowable_Increase = "", stringsAsFactors = FALSE)
  AllowableIncreaseValue <- AllowableIncreaseValue[-1, ]
  names(AllowableIncreaseValue)[-1:-2] <- c("Allowable Increase [%]", "Allowable Increase [absolute]")

  for (i in colnames(x$landUse)) {
    for (j in unique(x$scenarioTable$indicator)) {

      z <- 0

      tempresult <- solveScenario(x)

      # Schrittweises Erhöhen des Indikatorwertes um 1000% (+10) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
      while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))}) & z <= 50){

        z <- z + 10

        tempinit <- x
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)]* (1+z)

        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

        if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

        if(tempinit$scenarioSettings$optimisticRule == "expectation") {
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

        if(is.null(fixDistance)){
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
            apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
        tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
        tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
        tempresult <- solveScenario(tempinit)
        if (tempresult$status == "no optimum found") {break}
      }
      if (z >= 50) {
        AllowableIncreaseValue[nrow(AllowableIncreaseValue) + 1, ] <- c(i, j, "Inf", "Inf")
        next # überspringt die aktuelle Iteration
      } else {
        z <- z - 10
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 100% (+1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 1

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)]* (1+z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }
        z <- z - 1
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 10% (+0.1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 0.1

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)]* (1+z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }
        z <- z - 0.1
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 1% (+0.01) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 0.01

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)]* (1+z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }
        z <- z - 0.01
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 0.1% (+0.001) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 0.001

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)]* (1+z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }


        AllowableIncreaseValue[nrow(AllowableIncreaseValue) + 1, ] <- c(i, j, (z - 0.001)*100, round( unique(x$scenarioTable[x$scenarioTable$indicator == j, paste0("mean",i)])* (z-0.001), digits = 2))
      }}}

  AllowableIncreaseValue$`Allowable Increase [%]` <- as.numeric(AllowableIncreaseValue$`Allowable Increase [%]`)
  AllowableIncreaseValue$`Allowable Increase [absolute]` <- as.numeric(AllowableIncreaseValue$`Allowable Increase [absolute]`)


  #---------------------------------------#
  #### Allowable Increase Uncertainty  ####
  #---------------------------------------#

  # Erzeugen eines leeren data.frames
  AllowableIncreaseUncertainty <- data.frame(landUse = "",
                                             indicator = "",
                                             Allowable_Increase = "",
                                             Allowable_Increase = "", stringsAsFactors = FALSE)
  AllowableIncreaseUncertainty <- AllowableIncreaseUncertainty[-1, ]
  names(AllowableIncreaseUncertainty)[-1:-2] <- c("Allowable Increase [%]", "Allowable Increase [absolute]")

  for (i in colnames(x$landUse)) {
    for (j in unique(x$scenarioTable$indicator)) {

      z <- 0

      tempresult <- solveScenario(x)

      # Schrittweises Erhöhen des Indikatorwertes um 1000% (+10) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
      while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))}) & z <= 50){

        z <- z + 10

        tempinit <- x
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)]* (1+z)

        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

        if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

        if(tempinit$scenarioSettings$optimisticRule == "expectation") {
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

        if(is.null(fixDistance)){
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
            apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
        tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
        tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
        tempresult <- solveScenario(tempinit)
        if (tempresult$status == "no optimum found") {break}
      }
      if (z >= 50) {
        AllowableIncreaseUncertainty[nrow(AllowableIncreaseUncertainty) + 1, ] <- c(i, j, "Inf", "Inf")
        next # überspringt die aktuelle Iteration
      } else {
        z <- z - 10
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 100% (+1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 1

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)]* (1+z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }
        z <- z - 1
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 10% (+0.1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 0.1

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)]* (1+z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }
        z <- z - 0.1
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 1% (+0.01) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 0.01

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)]* (1+z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }
        z <- z - 0.01
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 0.1% (+0.001) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 0.001

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)]* (1+z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }


        AllowableIncreaseUncertainty[nrow(AllowableIncreaseUncertainty) + 1, ] <- c(i, j, (z - 0.001)*100, round( unique(x$scenarioTable[x$scenarioTable$indicator == j, paste0("sem",i)])* (z-0.001), digits = 2))
      }}}

  AllowableIncreaseUncertainty$`Allowable Increase [%]` <- as.numeric(AllowableIncreaseUncertainty$`Allowable Increase [%]`)
  AllowableIncreaseUncertainty$`Allowable Increase [absolute]` <- as.numeric(AllowableIncreaseUncertainty$`Allowable Increase [absolute]`)

  #--------------------------------#
  #### Allowable Decrease Value ####
  #--------------------------------#

  # Erzeugen eines leeren data.frames
  AllowableDecreaseValue <- data.frame(landUse = "",
                                       indicator = "",
                                       Allowable_Decrease = "",
                                       Allowable_Decrease = "", stringsAsFactors = FALSE)
  AllowableDecreaseValue <- AllowableDecreaseValue[-1, ]
  names(AllowableDecreaseValue)[-1:-2] <- c("Allowable Decrease [%]", "Allowable Decrease [absolute]")

  for (i in colnames(x$landUse)) {
    for (j in unique(x$scenarioTable$indicator)) {

      z <- 0

      tempresult <- solveScenario(x)

      # Schrittweises Erhöhen des Indikatorwertes um 1000% (+10) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
      while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))}) & z <= 50){

        z <- z + 10

        tempinit <- x
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)]* (1-z)

        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

        if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

        if(tempinit$scenarioSettings$optimisticRule == "expectation") {
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

        if(is.null(fixDistance)){
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
            apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
        tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
        tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
        tempresult <- solveScenario(tempinit)
        if (tempresult$status == "no optimum found") {break}
      }
      if (z >= 50) {
        AllowableDecreaseValue[nrow(AllowableDecreaseValue) + 1, ] <- c(i, j, "Inf", "Inf")
        next # überspringt die aktuelle Iteration
      } else {
        z <- z - 10
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 100% (+1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 1

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)]* (1-z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }
        z <- z - 1
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 10% (+0.1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 0.1

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)]* (1-z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }
        z <- z - 0.1
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 1% (+0.01) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 0.01

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)]* (1-z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }
        z <- z - 0.01
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 0.1% (+0.001) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 0.001

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)]* (1-z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }


        AllowableDecreaseValue[nrow(AllowableDecreaseValue) + 1, ] <- c(i, j, (z - 0.001)*100, round( unique(x$scenarioTable[x$scenarioTable$indicator == j, paste0("mean",i)])* (z-0.001), digits = 2))
      }}}

  AllowableDecreaseValue$`Allowable Decrease [%]` <- as.numeric(AllowableDecreaseValue$`Allowable Decrease [%]`)
  AllowableDecreaseValue$`Allowable Decrease [absolute]` <- as.numeric(AllowableDecreaseValue$`Allowable Decrease [absolute]`)


  #--------------------------------------#
  #### Allowable Decrease Uncertainty ####
  #--------------------------------------#

  # Erzeugen eines leeren data.frames
  AllowableDecreaseUncertainty <- data.frame(landUse = "",
                                             indicator = "",
                                             Allowable_Decrease = "",
                                             Allowable_Decrease = "", stringsAsFactors = FALSE)
  AllowableDecreaseUncertainty <- AllowableDecreaseUncertainty[-1, ]
  names(AllowableDecreaseUncertainty)[-1:-2] <- c("Allowable Decrease [%]", "Allowable Decrease [absolute]")

  for (i in colnames(x$landUse)) {
    for (j in unique(x$scenarioTable$indicator)) {

      z <- 0

      tempresult <- solveScenario(x)

      # Schrittweises Erhöhen des Indikatorwertes um 1000% (+10) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
      while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))}) & z <= 50){

        z <- z + 10

        tempinit <- x
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)]* (1-z)

        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

        if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

        if(tempinit$scenarioSettings$optimisticRule == "expectation") {
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

        if(is.null(fixDistance)){
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
            apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
        tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
        tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
        tempresult <- solveScenario(tempinit)
        if (tempresult$status == "no optimum found") {break}
      }
      if (z >= 50) {
        AllowableDecreaseUncertainty[nrow(AllowableDecreaseUncertainty) + 1, ] <- c(i, j, "Inf", "Inf")
        next # überspringt die aktuelle Iteration
      } else {
        z <- z - 10
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 100% (+1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 1

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)]* (1-z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }
        z <- z - 1
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 10% (+0.1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 0.1

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)]* (1-z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }
        z <- z - 0.1
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 1% (+0.01) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 0.01

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)]* (1-z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }
        z <- z - 0.01
        tempresult <- solveScenario(x)

        # Schrittweises Erhöhen des Indikatorwertes um 0.1% (+0.001) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})){

          z <- z + 0.001

          tempinit <- x
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("sem",i)]* (1-z)

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

          if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

          if(tempinit$scenarioSettings$optimisticRule == "expectation") {
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
              tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

          if(is.null(fixDistance)){
            tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
              apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
          tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
          tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
          tempresult <- solveScenario(tempinit)
          if (tempresult$status == "no optimum found") {break}
        }


        AllowableDecreaseUncertainty[nrow(AllowableDecreaseUncertainty) + 1, ] <- c(i, j, (z - 0.001)*100, round( unique(x$scenarioTable[x$scenarioTable$indicator == j, paste0("sem",i)])* (z-0.001), digits = 2))
      }}}

  AllowableDecreaseUncertainty$`Allowable Decrease [%]` <- as.numeric(AllowableDecreaseUncertainty$`Allowable Decrease [%]`)
  AllowableDecreaseUncertainty$`Allowable Decrease [absolute]` <- as.numeric(AllowableDecreaseUncertainty$`Allowable Decrease [absolute]`)

  #----------------#
  #### Increase ####
  #----------------#

  # Erzeugen eines leeren data.frames, in dem die Ergebnisse der Schleifendurchgänge abgespeichert werden
  Increase <- data.frame(landUse = "",
                         indicator = "",
                         matrix(ncol = 2 * length(unique(dat$landUse))), stringsAsFactors = FALSE)
  names(Increase)[-1:-2] <- paste0(colnames(x$landUse), "Value")
  names(Increase)[-1:-(2+length(colnames(x$landUse)))] <- paste0(colnames(x$landUse), "Uncertainty")
  Increase <- Increase[-1, ]


  # Erhöhen der einzelnen Inputwerte
  for (i in colnames(x$landUse)) {
    for (j in unique(x$scenarioTable$indicator)) {

      # Erhöhen des Indikatorwertes
      tempinit <- x
      tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)]* (1 + y)

      tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

      tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

      if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

      if(tempinit$scenarioSettings$optimisticRule == "expectation") {
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

      if(is.null(fixDistance)){
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
          apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
      tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
      tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
      tempresult <- solveScenario(tempinit)



      # Erhöhen der Unsicherheit
      tempinit2 <- x
      tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j, paste0("sem",i)] <- tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j, paste0("sem",i)]* (1 + y)

      tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit2$scenarioSettings$uValue

      tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit2$scenarioSettings$uValue

      if(tempinit2$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
          tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit2$scenarioSettings$uValue
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
          tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit2$scenarioSettings$uValue}

      if(tempinit2$scenarioSettings$optimisticRule == "expectation") {
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

      if(is.null(fixDistance)){
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
          apply(tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j, startsWith(names(tempinit2$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
      tempinit2$coefObjective <- defineObjectiveCoefficients(tempinit2$scenarioTable)
      tempinit2$coefConstraint <- defineConstraintCoefficients(tempinit2$scenarioTable)
      tempresult2 <- solveScenario(tempinit2)

      Increase[nrow(Increase) + 1, ] <- c(i, j, tempresult$landUse, tempresult2$landUse)
    }
  }

  # Runden der Ergebnisse
  Increase[ , -1:-2] <- round(Increase[ , -1:-2], digits = 3)


  # Überprüfen ob das Ergebnis der Interation dem Ausgangsergebnis entspricht
  Increase$sameResultValue <- apply(Increase[ ,endsWith(names(Increase), "Value")], 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})
  Increase$sameResultUncertainty <- apply(Increase[ ,endsWith(names(Increase), "Uncertainty")], 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})

  # Anordnen der Spalten
  Increase <- cbind(Increase[ , 1:2], Increase[ ,endsWith(names(Increase), "Value")], Increase[ ,endsWith(names(Increase), "Uncertainty")])



  #----------------#
  #### Decrease ####
  #----------------#

  # Erzeugen eines leeren data.frames, in dem die Ergebnisse der Schleifendurchgänge abgespeichert werden
  Decrease <- data.frame(landUse = "",
                         indicator = "",
                         matrix(ncol = 2 * length(unique(dat$landUse))), stringsAsFactors = FALSE)
  names(Decrease)[-1:-2] <- paste0(colnames(x$landUse), "Value")
  names(Decrease)[-1:-(2+length(colnames(x$landUse)))] <- paste0(colnames(x$landUse), "Uncertainty")
  Decrease <- Decrease[-1, ]


  # Erhöhen der einzelnen Inputwerte
  for (i in colnames(x$landUse)) {
    for (j in unique(x$scenarioTable$indicator)) {

      # Verringern des Indikatorwertes
      tempinit <- x
      tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)]* (1 - y)

      tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

      tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit$scenarioSettings$uValue

      if(tempinit$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit$scenarioSettings$uValue}

      if(tempinit$scenarioSettings$optimisticRule == "expectation") {
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "less is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit$scenarioTable[tempinit$scenarioTable$indicator == j & tempinit$scenarioTable$direction == "more is better" & tempinit$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

      if(is.null(fixDistance)){
        tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
          apply(tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, startsWith(names(tempinit$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
      tempinit$coefObjective <- defineObjectiveCoefficients(tempinit$scenarioTable)
      tempinit$coefConstraint <- defineConstraintCoefficients(tempinit$scenarioTable)
      tempresult <- solveScenario(tempinit)



      # Verringern der Unsicherheit
      tempinit2 <- x
      tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j, paste0("sem",i)] <- tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j, paste0("sem",i)]* (1 - y)

      tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit2$scenarioSettings$uValue

      tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * tempinit2$scenarioSettings$uValue

      if(tempinit2$scenarioSettings$optimisticRule == "uncertaintyAdjustedExpectation") {
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
          tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit2$scenarioSettings$uValue
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
          tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * tempinit2$scenarioSettings$uValue}

      if(tempinit2$scenarioSettings$optimisticRule == "expectation") {
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "less is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
          tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j & tempinit2$scenarioTable$direction == "more is better" & tempinit2$scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]}

      if(is.null(fixDistance)){
        tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
          apply(tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j, startsWith(names(tempinit2$scenarioTable), "adjSem")], 1, function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()}
      tempinit2$coefObjective <- defineObjectiveCoefficients(tempinit2$scenarioTable)
      tempinit2$coefConstraint <- defineConstraintCoefficients(tempinit2$scenarioTable)
      tempresult2 <- solveScenario(tempinit2)

      Decrease[nrow(Decrease) + 1, ] <- c(i, j, tempresult$landUse, tempresult2$landUse)
    }
  }

  # Runden der Ergebnisse
  Decrease[ , -1:-2] <- round(Decrease[ , -1:-2], digits = 3)


  # Überprüfen ob das Ergebnis der Interation dem Ausgangsergebnis entspricht
  Decrease$sameResultValue <- apply(Decrease[ ,endsWith(names(Decrease), "Value")], 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})
  Decrease$sameResultUncertainty <- apply(Decrease[ ,endsWith(names(Decrease), "Uncertainty")], 1, FUN = function(a){all(a == round(x$landUse, digits = digits))})

  # Anordnen der Spalten
  Decrease <- cbind(Decrease[ , 1:2], Decrease[ ,endsWith(names(Decrease), "Value")], Decrease[ ,endsWith(names(Decrease), "Uncertainty")])




#------------------------------#
#### Ergebnisse abspeichern ####
#------------------------------#

  x$AllowableIncreaseValue <- AllowableIncreaseValue
  x$AllowableIncreaseUncertainty <- AllowableIncreaseUncertainty
  x$AllowableDecreaseValue <- AllowableDecreaseValue
  x$AllowableDecreaseUncertainty <- AllowableDecreaseUncertainty
  x$Increase <- Increase
  x$Decrease <- Decrease

  return(x)
}

