
##--#############################--##
#### Solve a optimLandUse object ####
##--#############################--##
# Tue Jul  5 17:33:25 2022 ------------------------------

# Main developer: Kai Husmann

#' Perform the optimization
#'
#' The function solves the optimization framework specified by the initialized \emph{optimLanduse} object.
#'
#' The methodological background and the formulation of the optimization
#' framework are described in Knoke et al. (2016) and in Husmann et al. (2020)
#'
#' @param x The initialized \emph{optimLanduse} object. See \code{\link{initScenario}} for the initialization.
#' @param digitsPrecision Precision of the loss value. digitsPrecision is the
#' possibility to influence the calculation time.
#' @param lowerBound Optional lower bounds for the land-use options. Must be 0 or a vector in the dimension of the land-use options.
#' @param upperBound Optional upper bounds for the land-use options. Must be 1 or a vector in the dimension of the land-use options.
#' @return A solved landUse portfolio ready for export or further data processing.
#' @examples
#' require(readxl)
#' dat <- read_xlsx(exampleData("exampleGosling.xlsx"))
#' init <- initScenario(dat, uValue = 2,
#'                      optimisticRule = "expectation",
#'                      fixDistance = 3)
#' result <- solveScenario(x = init)
#'
#' @references Knoke, T., Paul, C., Hildebrandt, P. et al. (2016): Compositional diversity
#' of rehabilitated tropical lands supports multiple ecosystem services and
#' buffers uncertainties. \emph{Nat Commun} \strong{7}, 11877. \doi{10.1038/ncomms11877}
#'
#' @references Husmann, K., von Groß, V., Bödeker, K., Fuchs, J. M., Paul, C., & Knoke, T. (2022). optimLanduse: A package for multiobjective land-cover composition optimization under uncertainty. Methods in Ecology and Evolution, 00, 1– 10. https://doi.org/10.1111/2041-210X.14000


#' @import gurobi

#' @export
solveScenario <- function (x, digitsPrecision = 4,
                           lowerBound = 0, upperBound = 1,
                           constraintMatrix = NULL) {

  params <- list( OutputFlag=0,
                  presolve=0)
  # OutputFlag=0,
  # presolve=0,
  # LogFile="TSP.log"
  if ((!is.null(constraintMatrix) & any(c(lowerBound != 0, upperBound != 1)))) {
    warning("A constraint matrix and boundaries are defined. The constraint matrix overrides the boundaries settings.")
    lowerBound <- 0; upperBound <-  1
  }

  precision <- 1 / 10^(digitsPrecision)

  coefObjective <- x$coefObjective
  piConstraintCoefficients <- x$coefConstraint
  piConstraintRhs <- c(0, .6, 1)

  model <- list()
  model$A <- rbind(rep(1, length(coefObjective)), piConstraintCoefficients)
  model$obj <- coefObjective
  model$modelsense <- "max"
  model$rhs <- c(1, rep(piConstraintRhs[2], nrow(piConstraintCoefficients)))
  model$sense <- c("=", rep(">=", nrow(piConstraintCoefficients)))
  if (any(lowerBound > 0)) { # lower bounds
    model$lb <- lowerBound
  } else{model$lb <- rep(0, length(coefObjective))}
  if (any(upperBound < 1)) { # upper bounds
    model$lb <- upperBound
  } else{model$ub <- rep(Inf, length(coefObjective))}

  if(!is.null(constraintMatrix)) {
    for(i in c(1 : dim(constraintMatrix$lhs)[1])) {
      model$A <- rbind(model$A, t(constraintMatrix$lhs[i,]))
      model$sense <- c(model$sense, constraintMatrix$type[i])
      model$rhs <- c(model$rhs, constraintMatrix$rhs[i])
    }
  }

  result <- gurobi(model, params)
  (statusOpt <- result$status)

  counter <- 1
  emergencyStop <- 1000

  # Stepwise approximation loop
  while (counter < emergencyStop) {

    counter <- counter + 1
    #if (refreshCoef) {
    # tbd. Bisher platzhalter.
    # Hier könnten dynamische Parameter eingegeben werden
    #}

    if (statusOpt == "OPTIMAL") {
      piConstraintRhs <- c(piConstraintRhs[2], round((piConstraintRhs[2] + piConstraintRhs[3]) / 2, digitsPrecision), piConstraintRhs[3])
    } else {
      piConstraintRhs <- c(piConstraintRhs[1], round((piConstraintRhs[1] + piConstraintRhs[2]) / 2, digitsPrecision), piConstraintRhs[2])
    }

    if(is.null(constraintMatrix)) {
      model$rhs <- c(1, rep(piConstraintRhs[2], nrow(piConstraintCoefficients)))
    } else {
      model$rhs <- c(1, rep(piConstraintRhs[2], nrow(piConstraintCoefficients)), constraintMatrix$rhs)
    }

    result <- gurobi(model, params)
    (statusOpt <- result$status)

    #if(all(c(piConstraintRhs[3] - piConstraintRhs[2], piConstraintRhs[2] - piConstraintRhs[1]) <= precision)) { # Prüfen!
    if(piConstraintRhs[3] - piConstraintRhs[1] <= precision) {
      break()
    }
  }

  if(statusOpt != "OPTIMAL") {

    if(is.null(constraintMatrix)) {
      model$rhs <- c(1, rep(piConstraintRhs[1], nrow(piConstraintCoefficients)))
    } else {
      model$rhs <- c(1, rep(piConstraintRhs[1], nrow(piConstraintCoefficients)), constraintMatrix$rhs)
    }

    result <- gurobi(model, params)
    (statusOpt <- result$status)

    retPiConstraintRhs <- piConstraintRhs[1]
  } else {
    retPiConstraintRhs <- piConstraintRhs[2]
  }

  result_list <- list()
  if(statusOpt != "OPTIMAL") {
    cat(paste0("No optimum found. Status code "), statusOpt, " (see gurobi() {gurobi} documentation).")

    result_list$status <- "no optimum found"
    result_list$beta <- NA
    result_list$landUse[1, ] <- rep(NA, length(coefObjective))
  } else {
    result_list$status <- "optimized"
    result_list$beta <- 1 - round(retPiConstraintRhs, digitsPrecision)
    result_list$landUse[1, ] <- result$x
  }
  return(x)
}

