
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
#' framework are described in Knoke et al. (2016) and in Husmann et al. (2022)
#'
#' @param x The initialized \emph{optimLanduse} object. See \code{\link{initScenario}} for the initialization.
#' @param digitsPrecision Precision of the loss value. digitsPrecision is the
#' possibility to influence the calculation time.
#' @param lowerBound Optional lower bounds for the land-use options. Must be 0 or a vector in the dimension of the land-use options.
#' @param upperBound Optional upper bounds for the land-use options. Must be 1 or a vector in the dimension of the land-use options.
#' @return A solved landUse portfolio ready for export or further data processing.
#' @examples
#' @references Knoke, T., Paul, C., Hildebrandt, P. et al. (2016): Compositional diversity
#' of rehabilitated tropical lands supports multiple ecosystem services and
#' buffers uncertainties. \emph{Nat Commun} \strong{7}, 11877. \doi{10.1038/ncomms11877}
#'
#' @references Husmann, K., von Groß, V., Bödeker, K., Fuchs, J. M., Paul, C., & Knoke, T. (2022). optimLanduse: A package for multiobjective land-cover composition optimization under uncertainty. Methods in Ecology and Evolution, 00, 1– 10. https://doi.org/10.1111/2041-210X.14000
#' @import lpSolveAPI
#' @export
solveScenario <- function (x, digitsPrecision = 4,
                           lowerBound = 0, upperBound = 1,
                           landUseRestriction = NA,
                           paretoY = NA, paretoX = NA,
                           paretoMaxDistance = NA) {

  if(any(is.na(paretoY)) & any(is.na(paretoX))) {
    coefObjective <- x$coefObjective
    # coefConstraint comes out of initScenario as a character matrix (the
    # tibble->as.matrix step in defineConstraintCoefficients coerces). Convert
    # via storage.mode (~10x faster than the per-element apply(as.numeric)).
    piConstraintCoefficients <- x$coefConstraint[,-1]
    storage.mode(piConstraintCoefficients) <- "double"
  }

  if(any(!is.na(paretoY)) & any(is.na(paretoX)) | any(is.na(paretoY)) & any(!is.na(paretoX))) {
    stop ("Both, paretoy_y and paretoY either need specific indicator names or NA")
  }

  if(any(!is.na(paretoY)) & any(!is.na(paretoX))) {
    coefObjective <- defineObjectiveCoefficients(x$scenarioTable[x$scenarioTable$indicator %in% c(paretoY),])
    constraint_paretoY <- x$coefConstraint[x$coefConstraint[,1] %in% paretoY,]
    constraint_paretoX <- x$coefConstraint[x$coefConstraint[,1] %in% paretoX,]
    piConstraintCoefficients <- constraint_paretoY[,-1]
    storage.mode(piConstraintCoefficients) <- "double"
    paretoConstraint <- constraint_paretoX[,-1]
    storage.mode(paretoConstraint) <- "double"
  }

  if(any(!is.na(landUseRestriction)) & !all(names(landUseRestriction) %in% names(x$landUse))) {
    print(names(landUseRestriction)[!names(landUseRestriction) %in% names(x$landUse)])
    stop("The landUseRestriction argument must be a subset of the landUse options.")
  }

  # Single-LP reformulation: beta is decision variable #N+1.
  #
  #   max  beta
  #   s.t. sum_i x_i           = 1
  #        x_i                <= landUseRestriction_i        (optional)
  #        coef_s * x - beta  >= 0   for each scenario s
  #        coef_p * x         >= paretoMaxDistance           (optional Pareto)
  #
  # Replaces the prior bisection over beta (~14 solves of the same LP).
  # The matrix is built column-wise via set.column: 40k+ rows at N=12 made
  # per-row add.constraint() the dominant cost — column-wise is ~one C call
  # per variable instead of one per row. Phase 2 re-fixes beta and maximises
  # coefObjective * x so the returned portfolio matches the bisection version
  # at the same optimal beta.

  hasRestr  <- all(!is.na(landUseRestriction))
  hasPareto <- any(!is.na(paretoY)) & any(!is.na(paretoX))

  nLulc   <- length(coefObjective)
  betaIdx <- nLulc + 1
  nRestr  <- if (hasRestr)  length(landUseRestriction) else 0L
  nDist   <- nrow(piConstraintCoefficients)
  nPar    <- if (hasPareto) nrow(paretoConstraint)     else 0L
  nRows   <- 1L + nRestr + nDist + nPar

  if (hasRestr) {
    col_nums <- match(names(landUseRestriction), names(x$landUse))
  }

  # Assemble the full coefficient matrix in R, then push it to lpSolveAPI
  # column-wise. Avoids per-row R<->C overhead in the LP build.
  A <- matrix(0, nrow = nRows, ncol = nLulc + 1L)
  A[1L, seq_len(nLulc)] <- 1
  rowOff <- 1L
  if (hasRestr) {
    for (i in seq_len(nRestr)) {
      A[rowOff + i, col_nums[i]] <- 1
    }
    rowOff <- rowOff + nRestr
  }
  distRows <- (rowOff + 1L):(rowOff + nDist)
  A[distRows, seq_len(nLulc)] <- piConstraintCoefficients
  A[distRows, betaIdx]        <- -1
  rowOff <- rowOff + nDist
  if (hasPareto) {
    parRows <- (rowOff + 1L):(rowOff + nPar)
    A[parRows, seq_len(nLulc)] <- paretoConstraint
  }

  rhs <- numeric(nRows)
  rhs[1L] <- 1
  if (hasRestr) rhs[2L:(1L + nRestr)] <- landUseRestriction
  if (hasPareto) rhs[(nRows - nPar + 1L):nRows] <- paretoMaxDistance

  ctypes <- character(nRows)
  ctypes[1L] <- "="
  if (hasRestr) ctypes[2L:(1L + nRestr)] <- "<="
  ctypes[distRows] <- ">="
  if (hasPareto) ctypes[parRows] <- ">="

  lowerVec <- if (length(lowerBound) == 1) rep(lowerBound, nLulc) else lowerBound
  upperVec <- if (length(upperBound) == 1) rep(upperBound, nLulc) else upperBound

  lpaObj <- lpSolveAPI::make.lp(nrow = nRows, ncol = nLulc + 1L)
  # Populate the constraint matrix column-wise (one C call per variable, vs.
  # nRows calls for add.constraint). set.column zeroes the column's objective
  # coefficient, so set.objfn must come after the column loop.
  for (j in seq_len(nLulc + 1L)) {
    lpSolveAPI::set.column(lprec = lpaObj, column = j, x = A[, j])
  }
  # Phase 1 objective: maximize beta only.
  lpSolveAPI::set.objfn(lprec = lpaObj, obj = c(rep(0, nLulc), 1))
  lpSolveAPI::set.rhs(lprec = lpaObj, b = rhs)
  lpSolveAPI::set.constr.type(lprec = lpaObj, types = ctypes)
  lpSolveAPI::set.bounds(lprec = lpaObj,
                         lower = c(lowerVec, -1),
                         upper = c(upperVec,  2))
  lpSolveAPI::lp.control(lprec = lpaObj, sense = "max")

  statusOpt <- lpSolveAPI::solve.lpExtPtr(lpaObj)

  if (statusOpt != 0) {
    cat(paste0("No optimum found. Status code "), statusOpt,
        " (see solve.lpExtPtr {lpSolveAPI} documentation).")
    x$status <- "no optimum found"
    x$beta <- NA
    x$landUse[1, ] <- rep(NA, nLulc)
    return(x)
  }

  phase1Vars <- lpSolveAPI::get.variables(lpaObj)
  betaOpt    <- phase1Vars[betaIdx]

  # Phase 2: fix beta and maximise coefObjective * x to reproduce the original
  # tie-breaker (bisection ended with this same objective at the optimal beta).
  lpSolveAPI::set.bounds(lprec = lpaObj,
                         lower = c(lowerVec, betaOpt),
                         upper = c(upperVec, betaOpt))
  lpSolveAPI::set.objfn(lprec = lpaObj, obj = c(coefObjective, 0))

  statusOpt2 <- lpSolveAPI::solve.lpExtPtr(lpaObj)

  if (statusOpt2 == 0) {
    finalVars <- lpSolveAPI::get.variables(lpaObj)
    x$status  <- "optimized"
    x$beta    <- 1 - round(betaOpt, digitsPrecision)
    x$landUse[1, ] <- finalVars[seq_len(nLulc)]
  } else {
    # Phase 1 succeeded but phase 2 stumbled (numerically pathological case):
    # fall back to the phase-1 portfolio, which still satisfies all constraints
    # at the optimal beta.
    x$status <- "optimized"
    x$beta   <- 1 - round(betaOpt, digitsPrecision)
    x$landUse[1, ] <- phase1Vars[seq_len(nLulc)]
  }

  return(x)
}
