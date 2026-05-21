## Validate the rewritten autoSearch.R against an inlined copy of the
## original implementation. Both use the new solveScenario (the
## beta-reformulation - that change is independent).

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(tibble)
  library(lpSolveAPI); library(future); library(future.apply)
})

ROOT     <- "c:/Users/V/Downloads/optimLanduse"
BRANCH_A <- file.path(ROOT, "branches/feature_pareto_uncerSpace")

env <- new.env(parent = globalenv())
for (f in list.files(file.path(BRANCH_A, "R"), pattern = "\\.R$", full.names = TRUE)) {
  sys.source(f, envir = env)
}
A <- env

dat <- read_xlsx(file.path(BRANCH_A, "inst/extdata/exampleGosling.xlsx"))
lulc <- unique(dat$landUse)
obsLU <- data.frame(landUse = lulc, share = rep(1 / length(lulc), length(lulc)))

## ---------------------------------------------------------------------
## Original autoSearch, inlined for validation. Same logic as the
## pre-reformulation version (with the user's NA-filter patch).
## ---------------------------------------------------------------------
autoSearch_original <- function(coefTable, landUseObs, uValue = 1,
                                optimisticRule = "expectation",
                                fixDistance = 3) {
  landUseObs <- landUseObs[order(landUseObs$landUse), ]
  landUseObs <- as.vector(unlist(Filter(is.numeric, landUseObs)))
  coefTable  <- coefTable[order(coefTable$landUse), ]

  initMf   <- A$initScenario(coefTable, uValue = uValue,
                             optimisticRule = optimisticRule,
                             fixDistance = fixDistance)
  resultMf <- A$solveScenario(x = initMf)
  LandUseMf <- as.numeric(resultMf$landUse[, order(colnames(resultMf$landUse))])

  combInd <- do.call(c, lapply(seq_along(unique(coefTable$indicator)),
                               combn, x = unique(coefTable$indicator),
                               simplify = FALSE))
  names(combInd) <- rep("indicator", length(combInd))
  combList <- split(combInd, seq(length(combInd)))

  applyFun <- function(x, y) {
    ct   <- y[y$indicator %in% x$indicator, ]
    ct   <- ct[order(ct$landUse), ]
    init <- A$initScenario(ct, uValue = uValue,
                           optimisticRule = optimisticRule,
                           fixDistance = fixDistance)
    res  <- A$solveScenario(x = init)
    list(names(res$landUse), t(res$landUse), res$beta)
  }
  landUseResults <- future_lapply(combList, applyFun, y = coefTable,
                                  future.seed = TRUE)

  for (k in seq_along(combList)) {
    combList[[k]]$uValue         <- uValue
    combList[[k]]$LandUseOptions <- unlist(landUseResults[[k]][1], use.names = FALSE)
    combList[[k]]$result         <- unlist(landUseResults[[k]][2], use.names = FALSE)
    combList[[k]]$beta           <- unlist(landUseResults[[k]][3], use.names = FALSE)
    combList[[k]]$landUseObs     <- landUseObs
    combList[[k]]$LandUseMf      <- LandUseMf
  }
  combList <- Filter(function(x) !anyNA(x$result), combList)

  betaMFfun <- function(x) {
    list(A$solveScenario(initMf, lowerBound = x$result,
                         upperBound = x$result)$beta)
  }
  betaMf <- future_lapply(combList, betaMFfun, future.seed = TRUE)

  BcList     <- future_lapply(combList, function(x) {sum(abs(x$result - x$landUseObs)) / 2 * 100})
  BCList.two <- future_lapply(combList, function(x) {sum(abs(x$result - x$LandUseMf))  / 2 * 100})

  for (k in seq_along(combList)) {
    combList[[k]]$betaMf        <- unlist(betaMf[k], use.names = FALSE)
    combList[[k]]$BrayCurtisObs <- unlist(BcList[k], use.names = FALSE)
    combList[[k]]$BrayCurtisMf  <- unlist(BCList.two[k], use.names = FALSE)
  }
  bestResult    <- min(sapply(combList, function(x) x$BrayCurtisObs))
  bestResultObs <- Filter(function(x) x$BrayCurtisObs == bestResult, combList)
  list(bestResultObs, combList)
}

## ---------------------------------------------------------------------
## Run both, sequential plan
## ---------------------------------------------------------------------
plan(sequential)

cat("=== original (inlined) ===\n")
t1 <- system.time({
  r_old <- autoSearch_original(coefTable = dat, landUseObs = obsLU,
                               uValue = 2, optimisticRule = "expectation",
                               fixDistance = 3)
})
cat(sprintf("elapsed: %.2f s   subsets kept: %d\n", t1["elapsed"], length(r_old[[2]])))

cat("\n=== rewritten A$autoSearch ===\n")
t2 <- system.time({
  r_new <- A$autoSearch(coefTable = dat, landUseObs = obsLU,
                        uValue = 2, optimisticRule = "expectation",
                        fixDistance = 3)
})
cat(sprintf("elapsed: %.2f s   subsets kept: %d\n", t2["elapsed"], length(r_new[[2]])))

## ---------------------------------------------------------------------
## Compare per-subset
## ---------------------------------------------------------------------
keyfun <- function(x) paste(sort(x$indicator), collapse = "|")
old_by_key <- setNames(r_old[[2]], sapply(r_old[[2]], keyfun))
new_by_key <- setNames(r_new[[2]], sapply(r_new[[2]], keyfun))
common <- intersect(names(old_by_key), names(new_by_key))

cat(sprintf("\nsubsets in both: %d (old=%d, new=%d)\n",
            length(common), length(old_by_key), length(new_by_key)))

beta_max   <- 0
betaMf_max <- 0
share_max  <- 0
bc_obs_max <- 0
bc_mf_max  <- 0
for (k in common) {
  o <- old_by_key[[k]]; n <- new_by_key[[k]]
  beta_max   <- max(beta_max,   abs(o$beta   - n$beta),   na.rm = TRUE)
  betaMf_max <- max(betaMf_max, abs(o$betaMf - n$betaMf), na.rm = TRUE)
  share_max  <- max(share_max,  max(abs(o$result - n$result), na.rm = TRUE))
  bc_obs_max <- max(bc_obs_max, abs(o$BrayCurtisObs - n$BrayCurtisObs), na.rm = TRUE)
  bc_mf_max  <- max(bc_mf_max,  abs(o$BrayCurtisMf  - n$BrayCurtisMf),  na.rm = TRUE)
}
cat(sprintf("max |beta diff|:           %.1e\n", beta_max))
cat(sprintf("max |betaMf diff|:         %.1e\n", betaMf_max))
cat(sprintf("max |share diff|:          %.1e\n", share_max))
cat(sprintf("max |BrayCurtisObs diff|:  %.1e\n", bc_obs_max))
cat(sprintf("max |BrayCurtisMf  diff|:  %.1e\n", bc_mf_max))

best_old <- sapply(r_old[[1]], function(x) paste(x$indicator, collapse = ","))
best_new <- sapply(r_new[[1]], function(x) paste(x$indicator, collapse = ","))
cat(sprintf("\nbestResult indicator set(s):\n  old: %s\n  new: %s\n",
            paste(best_old, collapse = " | "),
            paste(best_new, collapse = " | ")))

cat(sprintf("\nspeedup: %.1fx\n", t1["elapsed"] / t2["elapsed"]))
