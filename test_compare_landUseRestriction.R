## Compare feature_pareto_uncerSpace (A) vs feature_pareto_uncerSpace_control (B)
## on the bundled exampleGosling.xlsx.
##
## A applies landUseRestriction as an LP upper-bound constraint in solveScenario().
## B applies landUseRestriction by rebuilding scenarioTable in initScenario() and
## transplanting normalization from the full-LULC version.
##
## Both intend to "exclude a LULC from the portfolio while keeping it in the
## uncertainty-space normalization". This test checks:
##   1. Do they yield the same beta and portfolio shares?
##   2. How do their runtimes compare (init+solve, and solve-only)?
##
## Dependencies: readxl, dplyr, tidyr, tibble, lpSolveAPI, microbenchmark
## Run from the project root (c:/Users/V/Downloads/optimLanduse).

library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(lpSolveAPI)
library(microbenchmark)

ROOT     <- "c:/Users/V/Downloads/optimLanduse"
BRANCH_A <- file.path(ROOT, "branches/feature_pareto_uncerSpace")
BRANCH_B <- file.path(ROOT, "branches/feature_pareto_uncerSpace_control")

## ---- Load each branch into its own environment ----------------------------
## Sourcing all R/*.R files of a branch into a single env gives the functions
## a shared enclosing scope. dplyr/tidyr/lpSolveAPI are resolved via the
## search path (attached above).

source_branch <- function(branch_dir) {
  env <- new.env(parent = globalenv())
  r_files <- list.files(file.path(branch_dir, "R"), pattern = "\\.R$", full.names = TRUE)
  for (f in r_files) sys.source(f, envir = env)
  env
}

A <- source_branch(BRANCH_A)
B <- source_branch(BRANCH_B)

## ---- Load example data ----------------------------------------------------

dat <- read_xlsx(file.path(BRANCH_A, "inst/extdata/exampleGosling.xlsx"))

lulc <- unique(dat$landUse)
cat("Available LULC options in exampleGosling.xlsx:\n")
print(lulc)

## Pick one LULC to forbid in the portfolio (but keep in uncertainty space).
## Edit EXCLUDED to test a different exclusion.
EXCLUDED <- "Silvopasture"
KEPT     <- setdiff(lulc, EXCLUDED)

cat(sprintf("\nExcluding from portfolio: %s\nKeeping:                  %s\n\n",
            EXCLUDED, paste(KEPT, collapse = ", ")))

## ---- Common settings ------------------------------------------------------
U_VALUE      <- 2
OPT_RULE     <- "expectation"
FIX_DISTANCE <- 3

## ---- Run A ----------------------------------------------------------------
initA <- A$initScenario(dat,
                        uValue = U_VALUE,
                        optimisticRule = OPT_RULE,
                        fixDistance = FIX_DISTANCE)
resA  <- A$solveScenario(initA,
                         landUseRestriction = setNames(0, EXCLUDED))

## ---- Run B ----------------------------------------------------------------
initB <- B$initScenario(dat,
                        uValue = U_VALUE,
                        optimisticRule = OPT_RULE,
                        fixDistance = FIX_DISTANCE,
                        landUseRestriction = KEPT)
resB  <- B$solveScenario(initB)

## ---- Compare beta ---------------------------------------------------------
cat("=== beta ===\n")
cat(sprintf("  A (LP-constraint):    %.10f\n", resA$beta))
cat(sprintf("  B (data-rebuild):     %.10f\n", resB$beta))
cat(sprintf("  difference (B - A):   %.3e\n", resB$beta - resA$beta))
cat(sprintf("  same to 1e-6?         %s\n",
            isTRUE(all.equal(resA$beta, resB$beta, tolerance = 1e-6))))
cat("  (Robust-LP theory predicts beta_B >= beta_A; equality is the best case.)\n\n")

## ---- Compare portfolio shares --------------------------------------------
portA <- as.numeric(resA$landUse); names(portA) <- names(resA$landUse)
portB <- as.numeric(resB$landUse); names(portB) <- names(resB$landUse)

all_lulc <- union(names(portA), names(portB))
side_by_side <- rbind(
  A = portA[match(all_lulc, names(portA))],
  B = portB[match(all_lulc, names(portB))]
)
colnames(side_by_side) <- all_lulc
side_by_side[is.na(side_by_side)] <- 0

cat("=== portfolio shares ===\n")
print(round(side_by_side, 6))

cat(sprintf("\n  max abs share difference: %.3e\n",
            max(abs(side_by_side["A", ] - side_by_side["B", ]))))
cat(sprintf("  same portfolio to 1e-6?   %s\n\n",
            isTRUE(all.equal(side_by_side["A", ], side_by_side["B", ],
                             tolerance = 1e-6, check.attributes = FALSE))))

## ---- Speed: init + solve --------------------------------------------------
cat("=== Speed: init + solve (single full pipeline) ===\n")
bench_full <- microbenchmark(
  A = {
    iA <- A$initScenario(dat, uValue = U_VALUE,
                         optimisticRule = OPT_RULE, fixDistance = FIX_DISTANCE)
    A$solveScenario(iA, landUseRestriction = setNames(0, EXCLUDED))
  },
  B = {
    iB <- B$initScenario(dat, uValue = U_VALUE,
                         optimisticRule = OPT_RULE, fixDistance = FIX_DISTANCE,
                         landUseRestriction = KEPT)
    B$solveScenario(iB)
  },
  times = 25
)
print(bench_full)

## ---- Speed: solve only (batch / sensitivity scenario) ---------------------
cat("\n=== Speed: solveScenario only (init pre-built) ===\n")
initA_pre <- A$initScenario(dat, uValue = U_VALUE,
                            optimisticRule = OPT_RULE, fixDistance = FIX_DISTANCE)
initB_pre <- B$initScenario(dat, uValue = U_VALUE,
                            optimisticRule = OPT_RULE, fixDistance = FIX_DISTANCE,
                            landUseRestriction = KEPT)
bench_solve <- microbenchmark(
  A_solve = A$solveScenario(initA_pre, landUseRestriction = setNames(0, EXCLUDED)),
  B_solve = B$solveScenario(initB_pre),
  times = 100
)
print(bench_solve)

cat("\nDone.\n")
