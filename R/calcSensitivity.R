
##--#################--##
#### calcSensitivity ####
##--#################--##
# Maintainer: Leona Ottens
# Developer: Leona Ottens, Kai Husmann, Volker von Groß

#' Some heading
#'
#' Text, short description
#' \code{\link{exampleData}} into to the expected format. The application of this function
#' is not mandatory
#' if you want to transform your data yourself or if your data is not formatted as
#' the example data. The application example on the asdf
#' \href{https://gitlab.gwdg.de/forest_economics_goettingen/optimlanduse}{GitLab project page}
#' provides information about the expected structure. Incomplete rows with NA-values are deleted and an error message is displayed.
#'
#' @param x Dat
#' @param y Weiterer Parameter

#' @examples
#' require(readxl)
#' dat <- read_xlsx(exampleData("exampleGosling_2020.xlsx"),
#'                  col_names = FALSE)
#' dat <- dataPreparation(dat, uncertainty = "SE", expVAL = "score")

#' @import dplyr

#' @export
calcSensitivity <- function(result, x = 0.01, digits = 3, fixDistance = NULL){


  #--------------------------------#
  #### Allowable Increase Value ####
  #--------------------------------#

  # Erzeugen eines leeren data.frames
  AllowableIncreaseValue <- data.frame(Iteration = "",
                                       Allowable_Increase = "",
                                       Allowable_Increase = "", stringsAsFactors = FALSE)
  AllowableIncreaseValue <- AllowableIncreaseValue[-1, ]
  names(AllowableIncreaseValue)[-1] <- c("Allowable Increase [%]", "Allowable Increase [absolute]")

  for (i in colnames(result$landUse)) {
    for (j in unique(result$scenarioTable$indicator)) {

      z <- 0

      tempresult <- solveScenario(result)

      # Schrittweises Erhöhen des Indikatorwertes um 1000% (+10) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
      while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))}) & z <= 50){

        z <- z + 10

        tempinit <- result
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
        AllowableIncreaseValue[nrow(AllowableIncreaseValue) + 1, ] <- c(paste0(i," | ", j), 5000, round( unique(result$scenarioTable[result$scenarioTable$indicator == j, paste0("mean",i)])* 50, digits = 2))
        next # überspringt die aktuelle Iteration
      } else {
        z <- z - 10
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 100% (+1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 1

          tempinit <- result
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
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 10% (+0.1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 0.1

          tempinit <- result
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
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 1% (+0.01) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 0.01

          tempinit <- result
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
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 0.1% (+0.001) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 0.001

          tempinit <- result
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


        AllowableIncreaseValue[nrow(AllowableIncreaseValue) + 1, ] <- c(paste0(i," | ", j), (z - 0.001)*100, round( unique(result$scenarioTable[result$scenarioTable$indicator == j, paste0("mean",i)])* (z-0.001), digits = 2))
      }}}

  #---------------------------------------#
  #### Allowable Increase Uncertainty  ####
  #---------------------------------------#

  # Erzeugen eines leeren data.frames
  AllowableIncreaseUncertainty <- data.frame(Iteration = "",
                                             Allowable_Increase = "",
                                             Allowable_Increase = "", stringsAsFactors = FALSE)
  AllowableIncreaseUncertainty <- AllowableIncreaseUncertainty[-1, ]
  names(AllowableIncreaseUncertainty)[-1] <- c("Allowable Increase [%]", "Allowable Increase [absolute]")

  for (i in colnames(result$landUse)) {
    for (j in unique(result$scenarioTable$indicator)) {

      z <- 0

      tempresult <- solveScenario(result)

      # Schrittweises Erhöhen des Indikatorwertes um 1000% (+10) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
      while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))}) & z <= 50){

        z <- z + 10

        tempinit <- result
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
        AllowableIncreaseUncertainty[nrow(AllowableIncreaseUncertainty) + 1, ] <- c(paste0(i," | ", j), 5000, round( unique(result$scenarioTable[result$scenarioTable$indicator == j, paste0("sem",i)])* 50, digits = 2))
        next # überspringt die aktuelle Iteration
      } else {
        z <- z - 10
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 100% (+1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 1

          tempinit <- result
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
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 10% (+0.1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 0.1

          tempinit <- result
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
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 1% (+0.01) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 0.01

          tempinit <- result
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
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 0.1% (+0.001) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 0.001

          tempinit <- result
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


        AllowableIncreaseUncertainty[nrow(AllowableIncreaseUncertainty) + 1, ] <- c(paste0(i," | ", j), (z - 0.001)*100, round( unique(result$scenarioTable[result$scenarioTable$indicator == j, paste0("sem",i)])* (z-0.001), digits = 2))
      }}}
  #--------------------------------#
  #### Allowable Decrease Value ####
  #--------------------------------#

  # Erzeugen eines leeren data.frames
  AllowableDecreaseValue <- data.frame(Iteration = "",
                                       Allowable_Decrease = "",
                                       Allowable_Decrease = "", stringsAsFactors = FALSE)
  AllowableDecreaseValue <- AllowableDecreaseValue[-1, ]
  names(AllowableDecreaseValue)[-1] <- c("Allowable Decrease [%]", "Allowable Decrease [absolute]")

  for (i in colnames(result$landUse)) {
    for (j in unique(result$scenarioTable$indicator)) {

      z <- 0

      tempresult <- solveScenario(result)

      # Schrittweises Erhöhen des Indikatorwertes um 1000% (+10) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
      while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))}) & z <= 50){

        z <- z + 10

        tempinit <- result
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
        AllowableDecreaseValue[nrow(AllowableDecreaseValue) + 1, ] <- c(paste0(i," | ", j), 5000, round( unique(result$scenarioTable[result$scenarioTable$indicator == j, paste0("mean",i)])* 50, digits = 2))
        next # überspringt die aktuelle Iteration
      } else {
        z <- z - 10
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 100% (+1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 1

          tempinit <- result
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
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 10% (+0.1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 0.1

          tempinit <- result
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
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 1% (+0.01) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 0.01

          tempinit <- result
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
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 0.1% (+0.001) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 0.001

          tempinit <- result
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


        AllowableDecreaseValue[nrow(AllowableDecreaseValue) + 1, ] <- c(paste0(i," | ", j), (z - 0.001)*100, round( unique(result$scenarioTable[result$scenarioTable$indicator == j, paste0("mean",i)])* (z-0.001), digits = 2))
      }}}

  #--------------------------------------#
  #### Allowable Decrease Uncertainty ####
  #--------------------------------------#

  # Erzeugen eines leeren data.frames
  AllowableDecreaseUncertainty <- data.frame(Iteration = "",
                                             Allowable_Decrease = "",
                                             Allowable_Decrease = "", stringsAsFactors = FALSE)
  AllowableDecreaseUncertainty <- AllowableDecreaseUncertainty[-1, ]
  names(AllowableDecreaseUncertainty)[-1] <- c("Allowable Decrease [%]", "Allowable Decrease [absolute]")

  for (i in colnames(result$landUse)) {
    for (j in unique(result$scenarioTable$indicator)) {

      z <- 0

      tempresult <- solveScenario(result)

      # Schrittweises Erhöhen des Indikatorwertes um 1000% (+10) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
      while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))}) & z <= 50){

        z <- z + 10

        tempinit <- result
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
        AllowableDecreaseUncertainty[nrow(AllowableDecreaseUncertainty) + 1, ] <- c(paste0(i," | ", j), 5000, round( unique(result$scenarioTable[result$scenarioTable$indicator == j, paste0("sem",i)])* 50, digits = 2))
        next # überspringt die aktuelle Iteration
      } else {
        z <- z - 10
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 100% (+1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 1

          tempinit <- result
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
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 10% (+0.1) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 0.1

          tempinit <- result
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
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 1% (+0.01) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 0.01

          tempinit <- result
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
        tempresult <- solveScenario(result)

        # Schrittweises Erhöhen des Indikatorwertes um 0.1% (+0.001) und überprüfen, ob das Ergebnis der Erhöhung dem eingentlichen Ergebnis des Optimierungproblems entspricht
        while(apply(round(tempresult$landUse, digits = digits), 1, FUN = function(x){all(x == round(result$landUse, digits = digits))})){

          z <- z + 0.001

          tempinit <- result
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


        AllowableDecreaseUncertainty[nrow(AllowableDecreaseUncertainty) + 1, ] <- c(paste0(i," | ", j), (z - 0.001)*100, round( unique(result$scenarioTable[result$scenarioTable$indicator == j, paste0("sem",i)])* (z-0.001), digits = 2))
      }}}

  #-----------------------------------#
  #### Einzelne Inputwerte erhöhen ####
  #-----------------------------------#

  # Erzeugen eines leeren data.frames, in dem die Ergebnisse der Schleifendurchgänge abgespeichert werden
  Increase <- data.frame(Iteration = "",
                         matrix(ncol = 2 * length(unique(dat$landUse))), stringsAsFactors = FALSE)
  names(Increase)[-1] <- paste0(colnames(result$landUse), "Value")
  names(Increase)[-1:-(1+length(colnames(result$landUse)))] <- paste0(colnames(result$landUse), "Uncertainty")
  Increase <- Increase[-1, ]


  # Erhöhen der einzelnen Inputwerte
  for (i in colnames(result$landUse)) {
    for (j in unique(result$scenarioTable$indicator)) {

      # Erhöhen des Indikatorwertes
      tempinit <- result
      tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)]* (1 + x)

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
      tempinit2 <- result
      tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j, paste0("sem",i)] <- tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j, paste0("sem",i)]* (1 + x)

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

      Increase[nrow(Increase) + 1, ] <- c(paste0(i," | ", j), tempresult$landUse, tempresult2$landUse)
    }
  }

  # Runden der Ergebnisse
  Increase[ , -1] <- round(Increase[ , -1], digits = 3)


  # Überprüfen ob das Ergebnis der Interation dem Ausgangsergebnis entspricht
  Increase$sameResultValue <- apply(Increase[ ,endsWith(names(Increase), "Value")], 1, FUN = function(x){all(x == round(result$landUse, digits = 3))})
  Increase$sameResultUncertainty <- apply(Increase[ ,endsWith(names(Increase), "Uncertainty")], 1, FUN = function(x){all(x == round(result$landUse, digits = 3))})

  # Anordnen der Spalten
  Increase <- cbind(Increase$Iteration, Increase[ ,endsWith(names(Increase), "Value")], Increase[ ,endsWith(names(Increase), "Uncertainty")])



  #--------------------------------------#
  #### Einzelne Inputwerte verringern ####
  #--------------------------------------#

  # Erzeugen eines leeren data.frames, in dem die Ergebnisse der Schleifendurchgänge abgespeichert werden
  Decrease <- data.frame(Iteration = "",
                         matrix(ncol = 2 * length(unique(dat$landUse))), stringsAsFactors = FALSE)
  names(Decrease)[-1] <- paste0(colnames(result$landUse), "Value")
  names(Decrease)[-1:-(1+length(colnames(result$landUse)))] <- paste0(colnames(result$landUse), "Uncertainty")
  Decrease <- Decrease[-1, ]


  # Erhöhen der einzelnen Inputwerte
  for (i in colnames(result$landUse)) {
    for (j in unique(result$scenarioTable$indicator)) {

      # Verringern des Indikatorwertes
      tempinit <- result
      tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)] <- tempinit$scenarioTable[tempinit$scenarioTable$indicator == j, paste0("mean",i)]* (1 - x)

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
      tempinit2 <- result
      tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j, paste0("sem",i)] <- tempinit2$scenarioTable[tempinit2$scenarioTable$indicator == j, paste0("sem",i)]* (1 - x)

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

      Decrease[nrow(Decrease) + 1, ] <- c(paste0(i, " | ", j), tempresult$landUse, tempresult2$landUse)
    }
  }

  # Runden der Ergebnisse
  Decrease[ , -1] <- round(Decrease[ , -1], digits = 3)


  # Überprüfen ob das Ergebnis der Interation dem Ausgangsergebnis entspricht
  Decrease$sameResultValue <- apply(Decrease[ ,endsWith(names(Decrease), "Value")], 1, FUN = function(x){all(x == round(result$landUse, digits = 3))})
  Decrease$sameResultUncertainty <- apply(Decrease[ ,endsWith(names(Decrease), "Uncertainty")], 1, FUN = function(x){all(x == round(result$landUse, digits = 3))})

  # Anordnen der Spalten
  Decrease <- cbind(Decrease$Iteration, Decrease[ ,endsWith(names(Decrease), "Value")], Decrease[ ,endsWith(names(Decrease), "Uncertainty")])




#------------------------------#
#### Ergebnisse abspeichern ####
#------------------------------#

  result$AllowableIncreaseValue <- AllowableIncreaseValue
  result$AllowableIncreaseUncertainty <- AllowableIncreaseUncertainty
  result$AllowableDecreaseValue <- AllowableDecreaseValue
  result$AllowableDecreaseUncertainty <- AllowableDecreaseUncertainty
  result$Increase <- Increase
  result$Decrease <- Decrease

  return(result)
}

