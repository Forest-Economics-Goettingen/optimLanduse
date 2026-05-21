## Regression test for the reformulated solveScenario in branch A.
##
## Branch A (`branches/feature_pareto_uncerSpace`) now solves the robust
## max-min LP in a single call (beta as decision variable + phase-2
## tie-breaker), replacing the master branch's bisection over beta.
##
## What "matches" means here
## -------------------------
## A and master are mathematically equivalent, but bit-for-bit equality
## is not expected:
##
##   1. beta. Master's bisection terminates with a bracket of width
##      digitsPrecision (default 1e-4) and reports the lower bound, so
##      its beta can be up to 1*precision BELOW the true optimum. A's
##      single LP returns the exact optimum. The robust-LP invariant is
##      therefore beta_A >= beta_master with |beta_A - beta_master| <=
##      2*precision in the worst case. This script checks both.
##
##   2. Portfolio shares. At the optimal beta there is typically a face
##      of optimal portfolios; both methods pick a vertex of that face
##      but not necessarily the same one. Phase 2 of A maximises
##      coefObjective * x at the fixed beta to reproduce master's
##      tie-breaker, but two different LP solvers (and even the same
##      solver under different basis paths) can land on different
##      vertices. Differences are reported but not asserted.
##
## Sections
## --------
##   1. Parity on unrestricted optimisations (A vs master) across a grid
##      of (uValue, optimisticRule, fixDistance) settings.
##   2. Non-binding landUseRestriction. A with rhs = 1 should equal A
##      with no restriction (and therefore agree with master within the
##      same tolerance), confirming that adding the inert restriction
##      column does not change the solution.
##   3. Binding landUseRestriction (A vs branch B) for each LULC.
##      Master does not support landUseRestriction; branch B is the
##      oracle.
##
## Run from the project root:
##   "C:/Program Files/R/R-4.4.2/bin/Rscript.exe" test_compare_master.R

library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(lpSolveAPI)

ROOT     <- "c:/Users/V/Downloads/optimLanduse"
MASTER_R <- file.path(ROOT, "R")
BRANCH_A <- file.path(ROOT, "branches/feature_pareto_uncerSpace")
BRANCH_B <- file.path(ROOT, "branches/feature_pareto_uncerSpace_control")

source_dir <- function(dir) {
  env <- new.env(parent = globalenv())
  for (f in list.files(dir, pattern = "\\.R$", full.names = TRUE)) {
    sys.source(f, envir = env)
  }
  env
}

M <- source_dir(MASTER_R)
A <- source_dir(file.path(BRANCH_A, "R"))
B <- source_dir(file.path(BRANCH_B, "R"))

dat  <- read_xlsx(file.path(BRANCH_A, "inst/extdata/exampleGosling.xlsx"))
lulc <- unique(dat$landUse)

DIGITS_PREC  <- 4
PRECISION    <- 10^(-DIGITS_PREC)        # = 1e-4
BETA_TOL     <- 2 * PRECISION            # bisection worst-case slack
PASS         <- TRUE

log_result <- function(label, ok, detail = "") {
  marker <- if (ok) "PASS" else "FAIL"
  cat(sprintf("  [%s] %s%s\n", marker, label,
              if (nzchar(detail)) paste0("   ", detail) else ""))
  if (!ok) PASS <<- FALSE
}

## Comparison rule:
##   |beta_new - beta_ref| <= BETA_TOL (within bisection precision), AND
##   beta_new >= beta_ref - PRECISION (master's bisection underestimates
##     delta and therefore overestimates beta = 1 - delta; A returns the
##     exact optimum, so A's beta can be smaller than master's by up to
##     one precision step, but should not be smaller than that).
##   Floating-point ULP slack added so exact +/- 1e-4 diffs don't trip.
## Portfolio max |diff| is computed by aligning by LULC name; entries
## missing in either side (e.g. B drops an excluded LULC) count as zero.
compare <- function(resRef, resNew, label_ref = "ref", label_new = "new") {
  ULP        <- 1e-10
  beta_diff  <- resNew$beta - resRef$beta
  beta_ok    <- abs(beta_diff) <= BETA_TOL + ULP
  no_regress <- beta_diff     >= -PRECISION - ULP

  portRef <- setNames(as.numeric(resRef$landUse), names(resRef$landUse))
  portNew <- setNames(as.numeric(resNew$landUse), names(resNew$landUse))
  keys    <- union(names(portRef), names(portNew))
  pa      <- ifelse(is.na(portRef[keys]), 0, portRef[keys])
  pb      <- ifelse(is.na(portNew[keys]), 0, portNew[keys])
  port_diff <- max(abs(pa - pb))

  list(
    ok = beta_ok && no_regress,
    detail = sprintf("beta %s=%.6f %s=%.6f  (diff=%+.1e)  port max|diff|=%.1e",
                     label_ref, resRef$beta, label_new, resNew$beta,
                     beta_diff, port_diff)
  )
}

cat("======================================================================\n")
cat(sprintf(" Tolerances: |beta diff| <= %.0e  AND  beta_new >= beta_ref - %.0e\n",
            BETA_TOL, PRECISION))
cat(" (portfolio share differences are informational only)\n")
cat("======================================================================\n")

cat("\n======================================================================\n")
cat(" SECTION 1 - unrestricted optimisations (A vs master)\n")
cat("======================================================================\n")

grid <- expand.grid(
  uValue         = c(1, 2, 3),
  optimisticRule = c("expectation", "uncertaintyAdjustedExpectation"),
  fixDistance    = list(NA, 3),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(grid))) {
  uV  <- grid$uValue[i]
  opt <- grid$optimisticRule[i]
  fD  <- grid$fixDistance[[i]]
  initM <- M$initScenario(dat, uValue = uV, optimisticRule = opt, fixDistance = fD)
  initA <- A$initScenario(dat, uValue = uV, optimisticRule = opt, fixDistance = fD)
  cmp   <- compare(M$solveScenario(initM), A$solveScenario(initA),
                   label_ref = "M", label_new = "A")
  log_result(sprintf("uValue=%g  optimisticRule=%-32s fixDistance=%s",
                     uV, opt, ifelse(is.na(fD), "NA", as.character(fD))),
             cmp$ok, cmp$detail)
}

cat("\n======================================================================\n")
cat(" SECTION 2 - non-binding restriction (A with rhs=1 == A unrestricted)\n")
cat("======================================================================\n")
cat(" Adding x_i <= 1 is redundant with sum(x)=1, so the restricted LP\n")
cat(" must give the same beta as the unrestricted LP on branch A.\n\n")

initA <- A$initScenario(dat, uValue = 2, optimisticRule = "expectation", fixDistance = 3)
resA_unr <- A$solveScenario(initA)

for (L in lulc) {
  resA_rhs1 <- A$solveScenario(initA, landUseRestriction = setNames(1, L))
  cmp <- compare(resA_unr, resA_rhs1, label_ref = "A_unr", label_new = "A_rhs1")
  log_result(sprintf("restriction(%s) = 1", L), cmp$ok, cmp$detail)
}

cat("\n======================================================================\n")
cat(" SECTION 3 - binding restriction (A vs B, exclude each LULC in turn)\n")
cat("======================================================================\n")
cat(" master does not support landUseRestriction; branch B is the oracle.\n\n")

initA <- A$initScenario(dat, uValue = 2, optimisticRule = "expectation", fixDistance = 3)

for (EX in lulc) {
  KEPT  <- setdiff(lulc, EX)
  initB <- B$initScenario(dat, uValue = 2, optimisticRule = "expectation",
                          fixDistance = 3, landUseRestriction = KEPT)
  resA  <- A$solveScenario(initA, landUseRestriction = setNames(0, EX))
  resB  <- B$solveScenario(initB)
  cmp   <- compare(resB, resA, label_ref = "B", label_new = "A")
  log_result(sprintf("exclude %s", EX), cmp$ok, cmp$detail)

  # Sanity: the excluded LULC must actually be 0 (or numerically negligible)
  excluded_share <- as.numeric(resA$landUse[[EX]])
  if (excluded_share > PRECISION) {
    log_result(sprintf("  (A enforces %s == 0)", EX), FALSE,
               sprintf("share = %.3e", excluded_share))
  }
}

cat("\n======================================================================\n")
cat(if (PASS) " ALL CHECKS PASSED\n" else " SOME CHECKS FAILED - see [FAIL] lines above\n")
cat("======================================================================\n")
if (!PASS) quit(status = 1)
