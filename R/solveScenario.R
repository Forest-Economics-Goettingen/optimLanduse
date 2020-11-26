
##--#############################--##
#### Solve a optimLandUse object ####
##--#############################--##
# Mon Jan 27 10:35:54 2020 ------------------------------

#' Perform the optimization
#'
#' The function solves the initialized *optimLanduse* object.
#'
#' @param x The initialized *optimLanduse* object.
#' @param digitsPrecision Precision.
#' @return A solved landUse portfolio ready for export or further data processing.

#' @import lpSolveAPI

#' @export
solveScenario <- function (x, digitsPrecision = 4, lowerBound = 0) {

  coefObjective <- x$coefObjective
  piConstraintCoefficients <- x$coefConstraint
  #tbd. Die Variablen sollte ich noch umbenennen. Von piConstraintCoefficients zu coefConstraint

  precision <- 1 / 10^(digitsPrecision)
  constraintCoef <- rbind(rep(1, length(coefObjective)), piConstraintCoefficients)
  constraintDirection <- c("==", rep(">=", dim(piConstraintCoefficients)[1]))
  piConstraintRhs <- c(0, .6, 1)
  piConstraintRhsFeasible <- rep(FALSE, 3)
  emergencyStop <- 1000

  # Init lpa Object
  lpaObj <- make.lp(nrow = 0, ncol = length(coefObjective))
  set.objfn(lprec = lpaObj, obj = coefObjective)
  add.constraint(lprec = lpaObj, xt = rep(1, length(coefObjective)),
                 type = "=", rhs = 1)
  apply(piConstraintCoefficients,
        1,
        function(x) {add.constraint(lprec = lpaObj, xt = x, type = ">=", rhs = piConstraintRhs[2])}
  )

  if(any(lowerBound >= 0)) { # lower bounds
    for(i in 1 : length(coefObjective)) {
      add.constraint(lprec = lpaObj, xt = diag(length(coefObjective))[, i], type = ">=", lowerBound[i])
    }
  }

  lp.control(lprec = lpaObj, sense = "max")
  counter <- 1 # 1 as the first iteration is outside the loop

  if(any(lowerBound >= 0)) {
    set.rhs(lprec = lpaObj, b = c(1, rep(piConstraintRhs[2], dim(piConstraintCoefficients)[1]), lowerBound))
  } else {
    set.rhs(lprec = lpaObj, b = c(1, rep(piConstraintRhs[2], dim(piConstraintCoefficients)[1])))
  }

  statusOpt <- lpSolveAPI::solve.lpExtPtr(lpaObj)

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
    if(any(lowerBound >= 0)) {
      set.rhs(lprec = lpaObj, b = c(1, rep(piConstraintRhs[2], dim(piConstraintCoefficients)[1]), lowerBound))
    } else {
      set.rhs(lprec = lpaObj, b = c(1, rep(piConstraintRhs[2], dim(piConstraintCoefficients)[1])))
    }


    statusOpt <- lpSolveAPI::solve.lpExtPtr(lpaObj)

    if(all(c(piConstraintRhs[3] - piConstraintRhs[2], piConstraintRhs[2] - piConstraintRhs[1]) <= precision)) {
      break()
    }
  }

  if(statusOpt == 2) {
    if(any(lowerBound >= 0)) {
      set.rhs(lprec = lpaObj, b = c(1, rep(piConstraintRhs[2], dim(piConstraintCoefficients)[1]), lowerBound))
    } else {
      set.rhs(lprec = lpaObj, b = c(1, rep(piConstraintRhs[2], dim(piConstraintCoefficients)[1])))
    }
    statusOpt <- lpSolveAPI::solve.lpExtPtr(lpaObj)
    retPiConstraintRhs <- piConstraintRhs[1]
  } else {
    retPiConstraintRhs <- piConstraintRhs[2]
  }

  if(statusOpt != 0) {
    cat("No optimum found.")
  }

  x$status <- "optimized"
  x$beta <- round(retPiConstraintRhs, digitsPrecision)
  x$landUse[1, ] <- get.variables(lpaObj)
  return(x)
}
