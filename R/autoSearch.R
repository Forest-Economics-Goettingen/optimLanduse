
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
#' contain the land-use options. The second column the respective shares.
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
#' # Subset to save computation time
#'
#' coefTable <- coefTable[coefTable$indicator %in% c("Long-term income",
#'                                                  "Liquidity",
#'                                                  "Protecting soil resources"), ]
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
#' @import future.apply
#' @import lpSolveAPI
#' @importFrom utils combn
#'
#' @export
autoSearch <- function(coefTable, landUseObs, uValue = 1,
                       optimisticRule = "expectation", fixDistance = 3) {

  landUseObs <- landUseObs[order(landUseObs$landUse), ]

  if (any(sort(unique(coefTable$landUse)) != landUseObs$landUse)) {
    stop("Error: Land-use option for the coefTable and landUseObs argument are not the same or in the wrong order")
  }

  landUseObs <- as.vector(unlist(Filter(is.numeric, landUseObs)))
  coefTable  <- coefTable[order(coefTable$landUse), ]

  #### Calculate optimal result for later BC comparison ####
  initMf    <- initScenario(coefTable, uValue = uValue,
                            optimisticRule = optimisticRule,
                            fixDistance = fixDistance)
  resultMf  <- solveScenario(x = initMf)
  LandUseMf <- as.numeric(resultMf$landUse[, order(colnames(resultMf$landUse))])

  #### Pre-compute pieces used per subset in Block 1 ####
  # Each subset is a row-filter of the full scenarioTable by indicator.
  # defineConstraintCoefficients and defineObjectiveCoefficients operate
  # row-wise, so we run them ONCE on the full table and then row-slice
  # per subset (instead of re-running initScenario every iteration).
  scenarioTable_full <- initMf$scenarioTable
  empty_landUse      <- initMf$landUse

  cc_full <- defineConstraintCoefficients(scenarioTable_full)

  # Per-row contributions to coefObjective. Subset coefObjective is then
  # just colSums over the rows belonging to that subset's indicators.
  adjSem_cols  <- grep("^adjSem", names(scenarioTable_full))
  adjSem_mat   <- as.matrix(scenarioTable_full[, adjSem_cols])
  storage.mode(adjSem_mat) <- "double"
  sign_per_row <- ifelse(scenarioTable_full$direction == "less is better", -1, 1)
  contrib_full <- (adjSem_mat * sign_per_row) /
                  scenarioTable_full$diffAdjSem * 100

  # Indicator -> row-index maps for the two matrices.
  rows_by_ind    <- split(seq_len(nrow(scenarioTable_full)),
                          scenarioTable_full$indicator)
  rows_cc_by_ind <- split(seq_len(nrow(cc_full)), cc_full[, "indicator"])

  #### Create all indicator combinations ####
  combInd <- do.call(c, lapply(seq_along(unique(coefTable$indicator)),
                               combn, x = unique(coefTable$indicator),
                               simplify = FALSE))
  names(combInd) <- rep("indicator", length(combInd))
  combList <- split(combInd, seq(length(combInd)))

  #### Block 1: per-subset row-slice + solveScenario (parallel) ####
  applyFun <- function(x) {
    obj_rows <- unlist(rows_by_ind[x$indicator],    use.names = FALSE)
    cc_rows  <- unlist(rows_cc_by_ind[x$indicator], use.names = FALSE)
    init_subset <- list(
      coefObjective  = colSums(contrib_full[obj_rows, , drop = FALSE]),
      coefConstraint = cc_full[cc_rows, , drop = FALSE],
      landUse        = empty_landUse
    )
    result <- solveScenario(x = init_subset)
    list(names(result$landUse), t(result$landUse), result$beta)
  }
  landUseResults <- future_lapply(combList, applyFun, future.seed = TRUE)

  # Adding results and information values to the list
  for (k in c(1 : length(combList))) {
    combList[[k]]$uValue         <- uValue
    combList[[k]]$LandUseOptions <- unlist(landUseResults[[k]][1], use.names = FALSE)
    combList[[k]]$result         <- unlist(landUseResults[[k]][2], use.names = FALSE)
    combList[[k]]$beta           <- unlist(landUseResults[[k]][3], use.names = FALSE)
    combList[[k]]$landUseObs     <- landUseObs
    combList[[k]]$LandUseMf      <- LandUseMf
  }

  # Drop subsets whose LP solve returned NA shares (infeasible). Skips
  # wasted work in Block 2 and avoids passing NA bounds to lpSolveAPI.
  n_before  <- length(combList)
  combList  <- Filter(function(x) !anyNA(x$result), combList)
  n_dropped <- n_before - length(combList)
  if (n_dropped > 0) {
    message("autoSearch: dropped ", n_dropped,
            " infeasible subset(s) of ", n_before, " total.")
  }

  #### Block 2: clamped solves with a REUSED LP (sequential) ####
  # The constraint matrix and rhs are identical for every iteration
  # (they all come from initMf); only the variable bounds change.
  # Build the LP once with lpSolveAPI directly, then per subset just
  # call set.bounds + solve + get.variables. Phase 2 is skipped because
  # clamping every x_i fixes the portfolio - phase 1 already returns the
  # unique answer (beta = min_s coef_s * x in zero pivots).
  #
  # Block 2 stays sequential: the lpSolveAPI handle is a C pointer
  # (externalptr) that does not survive crossing a worker process
  # boundary. With the reuse trick Block 2 runs in 1-2 seconds anyway,
  # below the threshold where parallelism would pay for its overhead.
  lpa     <- .buildBlock2LP(initMf)
  nLulc   <- length(initMf$coefObjective)
  betaIdx <- nLulc + 1L

  betaMf <- vector("list", length(combList))
  for (k in seq_along(combList)) {
    shares <- combList[[k]]$result
    lpSolveAPI::set.bounds(lpa,
                           lower = c(shares, -1),
                           upper = c(shares,  2))
    lpSolveAPI::solve.lpExtPtr(lpa)
    betaOpt    <- lpSolveAPI::get.variables(lpa)[betaIdx]
    betaMf[[k]] <- 1 - round(betaOpt, 4)
  }

  #### Bray-Curtis (cheap; sequential lapply) ####
  BcList     <- lapply(combList, function(x) {sum(abs(x$result - x$landUseObs)) / 2 * 100})
  BCList.two <- lapply(combList, function(x) {sum(abs(x$result - x$LandUseMf))  / 2 * 100})

  for (BCloop in c(1 : length(combList))) {
    combList[[BCloop]]$betaMf        <- betaMf[[BCloop]]
    combList[[BCloop]]$BrayCurtisObs <- BcList[[BCloop]]
    combList[[BCloop]]$BrayCurtisMf  <- BCList.two[[BCloop]]
  }

  ## Defence-in-depth: drop any subset that still has NA in BrayCurtis.
  ## After the Block-1 NA filter this should never remove anything.
  combList <- Filter(function(x) !all(is.na(x$BrayCurtisObs)), combList)
  combList <- Filter(function(x) !all(is.na(x$BrayCurtisMf)), combList)

  bestResult    <- min(sapply(combList, function(x) x$BrayCurtisObs))
  bestResultObs <- Filter(function(x) x$BrayCurtisObs == bestResult, combList)

  return(list(bestResultObs, combList))
}

## Build the Block-2 LP once from an init object. Internal helper for
## autoSearch; not exported. Mirrors the LP construction in
## solveScenario but stops at the "ready to set bounds + solve" point so
## the caller can re-clamp variables per subset and reuse the same LP.
.buildBlock2LP <- function(init) {
  coefObjective <- init$coefObjective
  piC <- init$coefConstraint[, -1]
  storage.mode(piC) <- "double"

  nLulc <- length(coefObjective)
  nDist <- nrow(piC)
  nRows <- 1L + nDist

  A <- matrix(0, nrow = nRows, ncol = nLulc + 1L)
  A[1L, seq_len(nLulc)]      <- 1
  A[2L:nRows, seq_len(nLulc)] <- piC
  A[2L:nRows, nLulc + 1L]    <- -1

  rhs    <- c(1, rep(0, nDist))
  ctypes <- c("=", rep(">=", nDist))

  lpa <- lpSolveAPI::make.lp(nrow = nRows, ncol = nLulc + 1L)
  for (j in seq_len(nLulc + 1L)) {
    lpSolveAPI::set.column(lpa, j, A[, j])
  }
  # set.objfn AFTER set.column - set.column zeros that column's
  # objective coefficient on this lpSolveAPI version.
  lpSolveAPI::set.objfn(lpa, obj = c(rep(0, nLulc), 1))   # max beta
  lpSolveAPI::set.rhs(lpa, b = rhs)
  lpSolveAPI::set.constr.type(lpa, types = ctypes)
  lpSolveAPI::lp.control(lpa, sense = "max")
  lpa
}
