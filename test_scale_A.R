## Scaling test for branch A (feature_pareto_uncerSpace) only.
## Builds synthetic 12- and 13-LULC datasets by duplicating Gosling LULCs
## with small perturbations, then times init + solve with one LULC excluded.

library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(lpSolveAPI)

ROOT     <- "c:/Users/V/Downloads/optimLanduse"
BRANCH_A <- file.path(ROOT, "branches/feature_pareto_uncerSpace")

source_branch <- function(branch_dir) {
  env <- new.env(parent = globalenv())
  r_files <- list.files(file.path(branch_dir, "R"), pattern = "\\.R$", full.names = TRUE)
  for (f in r_files) sys.source(f, envir = env)
  env
}

A <- source_branch(BRANCH_A)

dat <- read_xlsx(file.path(BRANCH_A, "inst/extdata/exampleGosling.xlsx"))

cat(sprintf("Base dataset: %d LULCs, %d indicators, %d rows\n",
            length(unique(dat$landUse)),
            length(unique(dat$indicator)),
            nrow(dat)))

## ---- Synthetic LULC extender ----------------------------------------------
## Recycle original LULCs into "_v2", "_v3" copies with +/- 5% perturbation
## on indicatorValue and indicatorUncertainty. Keeps indicator structure
## and direction intact (real LULC counts in published studies cap around 8-12).

extend_lulc <- function(coefTable, target_n, seed = 42) {
  set.seed(seed)
  orig_lulc <- unique(coefTable$landUse)
  n_orig <- length(orig_lulc)
  if (target_n <= n_orig) return(coefTable)
  extra_needed <- target_n - n_orig

  extra_rows <- vector("list", extra_needed)
  for (i in seq_len(extra_needed)) {
    base_lulc <- orig_lulc[((i - 1) %% n_orig) + 1]
    suffix    <- ((i - 1) %/% n_orig) + 2
    new_name  <- sprintf("%s_v%d", base_lulc, suffix)
    rows <- coefTable[coefTable$landUse == base_lulc, ]
    rows$landUse <- new_name
    rows$indicatorValue       <- rows$indicatorValue       * runif(nrow(rows), 0.95, 1.05)
    rows$indicatorUncertainty <- rows$indicatorUncertainty * runif(nrow(rows), 0.95, 1.05)
    extra_rows[[i]] <- rows
  }
  bind_rows(coefTable, bind_rows(extra_rows))
}

run_A <- function(coefTable, excluded) {
  n_lulc <- length(unique(coefTable$landUse))
  n_scen <- 2^n_lulc
  cat(sprintf("\n--- N = %d LULCs (%d scenarios per indicator) ---\n",
              n_lulc, n_scen))

  t_init <- system.time({
    init <- A$initScenario(coefTable,
                           uValue = 2,
                           optimisticRule = "expectation",
                           fixDistance = 3)
  })
  cat(sprintf("  initScenario: %6.2f s elapsed (%.2f s user)\n",
              t_init["elapsed"], t_init["user.self"]))
  cat(sprintf("    scenarioTable rows: %d\n", nrow(init$scenarioTable)))

  t_solve <- system.time({
    res <- A$solveScenario(init,
                           landUseRestriction = setNames(0, excluded))
  })
  cat(sprintf("  solveScenario: %6.2f s elapsed (%.2f s user)\n",
              t_solve["elapsed"], t_solve["user.self"]))
  cat(sprintf("    beta = %.4f, status = %s\n", res$beta, res$status))

  total <- t_init["elapsed"] + t_solve["elapsed"]
  cat(sprintf("  TOTAL: %6.2f s\n", total))

  list(init_sec = as.numeric(t_init["elapsed"]),
       solve_sec = as.numeric(t_solve["elapsed"]),
       total_sec = as.numeric(total),
       beta = res$beta,
       shares = res$landUse)
}

## ---- Run at N = 6 (baseline), 8, 10, 12, 13 --------------------------------
results <- list()

for (N in c(6, 8, 10, 12, 13)) {
  dat_N <- extend_lulc(dat, N)
  excluded <- if ("Silvopasture" %in% unique(dat_N$landUse)) "Silvopasture" else unique(dat_N$landUse)[1]
  results[[as.character(N)]] <- run_A(dat_N, excluded)
}

## ---- Summary table ---------------------------------------------------------
cat("\n=== Scaling summary (branch A) ===\n")
summary_df <- data.frame(
  N           = as.integer(names(results)),
  scenarios   = 2^as.integer(names(results)),
  init_sec    = sapply(results, `[[`, "init_sec"),
  solve_sec   = sapply(results, `[[`, "solve_sec"),
  total_sec   = sapply(results, `[[`, "total_sec"),
  beta        = sapply(results, `[[`, "beta")
)
print(summary_df, row.names = FALSE)

cat("\nDone.\n")
