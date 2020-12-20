
##--#############################--##
#### Solve a optimLandUse object ####
##--#############################--##
# Thu Nov 26 09:20:45 2020 ------------------------------

#' Perform the optimization
#'
#' The function solves the initialized *optimLanduse* object.
#'
#' @param x The initialized *optimLanduse* object.
#' @param digitsPrecision Precision.
#' @param lowerBound An optional vector lower bounds for the land-use options. Must be in the dimension of the land-use options if delivered.
#' @param upperBound An optional vector upper bounds for the land-use options. Must be in the dimension of the land-use options if delivered.
#' @return A solved landUse portfolio ready for export or further data processing.

#' @import lpSolveAPI

#' @export
solveScenario <- function (x, digitsPrecision = 4, lowerBound = 0, upperBound = 1) {

  coefObjective <- x$coefObjective # Summen aus aller Scenarien der LandUse-Optionen (s. helper Funktion)
  piConstraintCoefficients <- x$coefConstraint # relativie Werte (m. Distanz als Divisor)
  #tbd. Die Variablen sollte ich noch umbenennen. Von piConstraintCoefficients zu coefConstraint

  precision <- 1 / 10^(digitsPrecision) # maximale Differenz zwischen 1. und 3. Wert von piConstraintRhs
  # constraintCoef <- rbind(rep(1, length(coefObjective)), piConstraintCoefficients) # wird nicht mehr genutzt
  constraintDirection <- c("==", rep(">=", dim(piConstraintCoefficients)[1])) # maximierung
  piConstraintRhs <- c(0, .6, 1) # quasi das "beta" das immer enger und enger wird
  # piConstraintRhsFeasible <- rep(FALSE, 3) # Variable wird nicht mehr verwendet
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

  statusOpt <- lpSolveAPI::solve.lpExtPtr(lpaObj) # Was bedeutet "ExtPtr"?

  # ein gutes Beispiel zum Lernen: https://rpubs.com/nayefahmad/linear-programming

  # Stepwise approximation loop
  while (counter < emergencyStop) {
    solutionFeasible <- TRUE

    counter <- counter + 1
    #if (refreshCoef) {
    # tbd. Bisher platzhalter.

    #}

    if (statusOpt == 0) {
      piConstraintRhs <- c(piConstraintRhs[2], round((piConstraintRhs[2] + piConstraintRhs[3]) / 2, digitsPrecision), piConstraintRhs[3])
    } else {
      piConstraintRhs <- c(piConstraintRhs[1], round((piConstraintRhs[1] + piConstraintRhs[2]) / 2, digitsPrecision), piConstraintRhs[2])
    }

    lpSolveAPI::set.rhs(lprec = lpaObj, b = c(1, rep(piConstraintRhs[2], dim(piConstraintCoefficients)[1])))


    (statusOpt <- lpSolveAPI::solve.lpExtPtr(lpaObj))

    #if(all(c(piConstraintRhs[3] - piConstraintRhs[2], piConstraintRhs[2] - piConstraintRhs[1]) <= precision)) { # Prüfen!
    if(piConstraintRhs[3] - piConstraintRhs[1] <= precision) {
      break()
    }
  }


  if(statusOpt == 2) {

    lpSolveAPI::set.rhs(lprec = lpaObj, b = c(1, rep(piConstraintRhs[1], dim(piConstraintCoefficients)[1])))  # Prüfen!!

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
    x$beta <- round(retPiConstraintRhs, digitsPrecision)
    x$landUse[1, ] <- lpSolveAPI::get.variables(lpaObj)
  }
  return(x)
}
