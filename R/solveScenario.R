
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


#' @import lpSolveAPI

#' @export
solveScenario <- function (x, digitsPrecision = 4,
                           lowerBound = 0, upperBound = 1,
                           constraintMatrix = NULL) {

  coefObjective <- x$coefObjective # Summen aus aller Scenarien der LandUse-Optionen (s. helper Funktion)
  piConstraintCoefficients <- x$coefConstraint # relativie Werte (m. Distanz als Divisor)
  #tbd. Die Variablen sollte ich noch umbenennen. Von piConstraintCoefficients zu coefConstraint

  if ((!is.null(constraintMatrix) & any(c(lowerBound != 0, upperBound != 1)))) {
    warning("A constraint matrix and boundaries are defined. The constraint matrix overrides the boundaries settings.")
    lowerBound <- 0; upperBound <-  1
  }

  precision <- 1 / 10^(digitsPrecision)
  # constraintCoef <- rbind(rep(1, length(coefObjective)), piConstraintCoefficients)
  constraintDirection <- c("==", rep(">=", dim(piConstraintCoefficients)[1]))
  piConstraintRhs <- c(0, .6, 1) # ein Vektor mit Werten für "beta", der immer enger und enger wird
  # piConstraintRhsFeasible <- rep(FALSE, 3)
  emergencyStop <- 1000

  # Init lpa Object
  lpaObj <- lpSolveAPI::make.lp(nrow = 0, ncol = length(coefObjective))
  lpSolveAPI::set.objfn(lprec = lpaObj, obj = coefObjective)
  lpSolveAPI::add.constraint(lprec = lpaObj, xt = rep(1, length(coefObjective)),
                 type = "=", rhs = 1)
  apply(piConstraintCoefficients,
        1,
        function(x) {lpSolveAPI::add.constraint(lprec = lpaObj, xt = x, type = ">=", rhs = piConstraintRhs[2])}
  )

  if (any(lowerBound > 0)) { # lower bounds
    # tbd. Hier fehlen noch mehrere Prüfungen (Anzahl und Reihenfolge der Optionen, Summe der Restiktionen < 1)
    lpSolveAPI::set.bounds(lprec = lpaObj, lower = lowerBound)
  }
  if (any(upperBound < 1)) { # upper bounds
    # tbd. s. o.
    lpSolveAPI::set.bounds(lprec = lpaObj, upper = upperBound)
  }

  lpSolveAPI::lp.control(lprec = lpaObj, sense = "max")
  counter <- 1 # 1 as the first iteration is outside the loop

  # Update the right hand side
  lpSolveAPI::set.rhs(lprec = lpaObj, b = c(1, rep(piConstraintRhs[2], dim(piConstraintCoefficients)[1])))

  # Update the restrictions if a constraint matrix is delivered.
  if(!is.null(constraintMatrix)) {
    for(i in c(1 : dim(constraintMatrix$lhs)[1])) { # modified 2022-11-21
      lpSolveAPI::add.constraint(lprec = lpaObj,
                                 xt = t(constraintMatrix$lhs[i,]),
                                 type = constraintMatrix$type[i],
                                 rhs = constraintMatrix$rhs[i])
    }
  }

  statusOpt <- lpSolveAPI::solve.lpExtPtr(lpaObj)
  # ein gutes Beispiel zum Lernen: https://rpubs.com/nayefahmad/linear-programming

  # Stepwise approximation loop
  while (counter < emergencyStop) {
    solutionFeasible <- TRUE

    counter <- counter + 1
    #if (refreshCoef) {
    # tbd. Bisher platzhalter.
    # Hier könnten dynamische Parameter eingegeben werden
    #}

    if (statusOpt == 0) {
      piConstraintRhs <- c(piConstraintRhs[2], round((piConstraintRhs[2] + piConstraintRhs[3]) / 2, digitsPrecision), piConstraintRhs[3])
    } else {
      piConstraintRhs <- c(piConstraintRhs[1], round((piConstraintRhs[1] + piConstraintRhs[2]) / 2, digitsPrecision), piConstraintRhs[2])
    }

    if(is.null(constraintMatrix)) {
      lpSolveAPI::set.rhs(lprec = lpaObj, b = c(1, rep(piConstraintRhs[2], dim(piConstraintCoefficients)[1])))
    } else {
      lpSolveAPI::set.rhs(lprec = lpaObj, b = c(1, rep(piConstraintRhs[2], dim(piConstraintCoefficients)[1]), constraintMatrix$rhs))
    }


    (statusOpt <- lpSolveAPI::solve.lpExtPtr(lpaObj))

    #if(all(c(piConstraintRhs[3] - piConstraintRhs[2], piConstraintRhs[2] - piConstraintRhs[1]) <= precision)) { # Prüfen!
    if(piConstraintRhs[3] - piConstraintRhs[1] <= precision) {
      break()
    }
  }


  if(statusOpt == 2) {

    if(is.null(constraintMatrix)) {
      lpSolveAPI::set.rhs(lprec = lpaObj, b = c(1, rep(piConstraintRhs[1], dim(piConstraintCoefficients)[1])))  # Pruefen!!
    } else {
      lpSolveAPI::set.rhs(lprec = lpaObj, b = c(1, rep(piConstraintRhs[1], dim(piConstraintCoefficients)[1]), constraintMatrix$rhs))
    }

    statusOpt <- lpSolveAPI::solve.lpExtPtr(lpaObj)
    retPiConstraintRhs <- piConstraintRhs[1]
  } else {
    retPiConstraintRhs <- piConstraintRhs[2]
  }


  if(statusOpt != 0) {
    cat(paste0("No optimum found. Status code "), statusOpt, " (see solve.lpExtPtr {lpSolveAPI} documentation).")
    x$status <- "no optimum found"
    x$beta <- NA
    x$landUse[1, ] <- rep(NA, length(coefObjective))
  } else {
    x$status <- "optimized"
    x$beta <- 1 - round(retPiConstraintRhs, digitsPrecision)
    x$landUse[1, ] <- lpSolveAPI::get.variables(lpaObj)
  }
  return(x)
}
