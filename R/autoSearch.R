
##--##########################################################--##
#### Generates a list of all possible indicator combinations  ####
##--##########################################################--##

# Sun Feb 12 08:34:11 2023 ------------------------------

# Main developer: Volker von Groß

#' Optimize all possible indicator combinations
#'
#' This iterative search function generates a list of all possible indicator combinations.
#' All indicator combinations are converted into a list format, where each
#' combination corresponds to a list entry. For each of these list entries, an
#' optimization is performed using the \emph{initScenario} and
#' \emph{solveScenario} functions of the package. How these functions work
#' in detail (incl. example code) can be seen in the help of the respective
#' function in the package and the README from Husmann et al. (2022).
#' The results are entirely saved into the respective list entry. In addition, each entry is
#' appended with the currently observed land-use portfolio and the land-use
#' portfolio when all indicators are optimized together. Out of this list, we
#' use the Bray-Curtis measure of dissimilarity to identify the indicators driving current
#' land-use decisions. An example and further explanation is given in the README
#' \href{https://github.com/Forest-Economics-Goettingen/optimLanduse/}{GitHub project page}
#'
#' @param coefTable Coefficient table in the expected \emph{optimLanduse} format.
#' @param landUseObs Data frame with two columns. The first column has to
#' contain the land-use options. The second colum the respective shares.
#' @param uValue \emph{u} Value. The uncertainty value delivered in the coefTable is
#' multiplied with this u value. The value, therefore, enables scenario analyses with differing
#' uncertainties in relation to indicator values. Higher u values can be interpreted as a higher
#' risk aversion of the decision maker.
#' @param optimisticRule Either \emph{expectation} or \emph{uncertaintyAdjustedExpectation}.
#' The rule indicates whether the optimistic outcomes of an indicator are directly
#' reflected by their expectations or if the indicator is calculated as expectation +
#' uncertainty when "more is better" or expectation - uncertainty respectively when "less is better".
#' An optimization based on \emph{expectation} considers only downside risks.
#' @param fixDistance This optional numeric value allows to define distinct uncertainty levels for the
#' calculation of the uncertainty space and the averaged distances of a certain land-cover composition
#' (see Equation 9 in Husmann et al. (2020)). Passing NA disables fixDistance. In this case,
#' the uncertainty space is defined by uValue.
#' @return A list with all possible indicator combinations, their respective optimization results and the
#' indicator set best describing the observed land-use decision.
#' @references Husmann, K., von Groß, V., Bödeker, K., Fuchs, J. M., Paul, C., & Knoke, T. (2022). optimLanduse: A package for multiobjective land-cover composition optimization under uncertainty. Methods in Ecology and Evolution, 00, 1– 10. https://doi.org/10.1111/2041-210X.14000
#' @examples
#' require(readxl)
#' require(future.apply)
#' plan(multisession)
#'
#' coefTable <- read_xlsx(exampleData("exampleGosling.xlsx"))
#'
#'
#' obsLU <- data.frame(landUse = c("Pasture", "Crops", "Forest", "Plantation",
#'                                 "Alley Cropping", "Silvopasture"),
#'                     share = c(0.59, 0.26, 0.14, 0.01, 0, 0))
#'
#' combList <- autoSearch(coefTable = coefTable,
#'                        landUseObs = obsLU,
#'                        uValue = 2,
#'                        optimisticRule = "expectation",
#'                        fixDistance = 3)
#' plan(sequential)
#'
#' @import future
#' @import future.apply
#' @importFrom utils combn
#'
#' @export
autoSearch <- function(coefTable, landUseObs, uValue = 1, optimisticRule = "expectation", fixDistance = 3){

  landUseObs <- landUseObs[order(landUseObs$landUse),]

  if(any(sort(unique(coefTable$landUse)) != landUseObs$landUse)){stop("Error: Land-use option for the coefTable and landUseObs argument are not the same or in the wrong order")}

  landUseObs <- as.vector(unlist(Filter(is.numeric, landUseObs)))

  coefTable <- coefTable[order(coefTable$landUse),]

  #### Calculate optimal result for later BC comparison ####
  initMf <- initScenario(coefTable, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
  resultMf <- solveScenario(x = initMf)
  LandUseMf<- as.numeric(resultMf$landUse[, order(colnames(resultMf$landUse))])

  #### Create all indicator combinations ####
  combInd <- do.call(c, lapply(seq_along(unique(coefTable$indicator)),
                               combn, x = unique(coefTable$indicator), simplify = FALSE))
  names(combInd) <- rep("indicator", length(combInd))
  combList <- split(combInd, seq(length(combInd)))

  #### Create applyFun-function for the future_lappy function ####
  applyFun <- function(x, y) {
    coefTable <- y[y$indicator %in% x$indicator,]
    coefTable <- coefTable[order(coefTable$landUse),]
    init <- initScenario(coefTable, uValue = uValue, optimisticRule = optimisticRule, fixDistance = fixDistance)
    result <- solveScenario(x = init)
    return(list(names(result$landUse), t(result$landUse), result$beta))
  }

  #### Calculation of the results for every indicator combination
  landUseResults <- future_lapply(combList, applyFun, y = coefTable)

  # Adding results and information values to the list
  for(k in c(1 : length(combList))){
    combList[[k]]$uValue <- uValue
    combList[[k]]$LandUseOptions <- unlist(landUseResults[[k]][1], use.names = FALSE)
    combList[[k]]$result <- unlist(landUseResults[[k]][2], use.names = FALSE)
    combList[[k]]$beta <- unlist(landUseResults[[k]][3], use.names = FALSE)
    combList[[k]]$landUseObs <- landUseObs
    combList[[k]]$LandUseMf <- LandUseMf
  }
  betaMFfun <- function(x) {
    result <- solveScenario(initMf, lowerBound = x$result,
                            upperBound = x$result)
    return(list(result$beta))
  }
  betaMf <- future_lapply(combList, betaMFfun)

  #### Add Bray-Curtis to every list entry ####
  BcList <- future_lapply(combList, function(x){sum(abs(x$result - x$landUseObs)) / 2 * 100})
  BCList.two <- future_lapply(combList, function(x){sum(abs(x$result - x$LandUseMf)) / 2 * 100})

  for(BCloop in c(1 : length(combList))){
    combList[[BCloop]]$betaMf <- unlist(betaMf[BCloop], use.names = FALSE)
    combList[[BCloop]]$BrayCurtisObs <- unlist(BcList[BCloop], use.names = FALSE)
    combList[[BCloop]]$BrayCurtisMf <- unlist(BCList.two[BCloop], use.names = FALSE)
  }

  ## Delete NA (No Optimum Found)
  combList <- Filter(function(x) !all(is.na(x$BrayCurtisObs)), combList)
  combList <- Filter(function(x) !all(is.na(x$BrayCurtisMf)), combList)

  bestResult <- min(sapply(combList, function(x) x$BrayCurtisObs))
  bestResultObs <- Filter(function(x) x$BrayCurtisObs == bestResult, combList)


  return(list(bestResultObs, combList))
}
