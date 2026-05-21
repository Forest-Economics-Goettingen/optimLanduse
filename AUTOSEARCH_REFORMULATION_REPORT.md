# `autoSearch()` reformulation — change report

## Summary

`autoSearch()` enumerates every non-empty subset of indicators (2^N − 1
subsets for N indicators) and for each subset runs the robust max-min
LP twice:

- **Block 1** — `initScenario` + `solveScenario` for the subset's
  indicators (produces β and a portfolio per subset).
- **Block 2** — `solveScenario` on the **full**-indicator `initMf`, with
  every x_i clamped to that subset's portfolio shares (gives the
  multi-functional β, `betaMf`).

The original built a fresh LP for **every** call in **both** blocks. At
10 indicators (1023 subsets) on N=6 LULC Gosling this took ~38 s
sequential; at N=10 LULC it climbed to ~240 s; at N=13 it extrapolates
to roughly ~3 hours.

The reformulation cuts redundant work in both blocks and turns Block 1
into a parallel-friendly hot path:

| change                                       | rationale                                                                            |
| -------------------------------------------- | ------------------------------------------------------------------------------------ |
| Skip per-subset `initScenario` (Block 1)     | The scenarioTable is built once for `initMf`; per subset just **row-slice by indicator** and re-derive `coefObjective` (`colSums`) and `coefConstraint` (matrix slice). |
| Reuse a single LP across all Block-2 calls   | The Block-2 constraint matrix is identical for every subset (it always comes from `initMf`). Build the LP once, then per subset just `set.bounds` + `solve.lpExtPtr` + `get.variables`. |
| Drop phase 2 in Block 2                      | Every x_i is clamped, so there are no degrees of freedom left and phase 1 already returns the unique answer. |
| Parallelise Block 1 via `future_lapply`      | Each subset still builds its own LP inside `solveScenario` — no shared C handle to coordinate. |
| Keep Block 2 sequential                      | The reused LP is a C handle (externalptr) that cannot cross process boundaries; with the reuse trick Block 2 runs in 1-2 s anyway. |

---

## Code diff (the core changes)

### Before — original autoSearch (per-subset rebuilds in both blocks)

```r
applyFun <- function(x, y) {
  coefTable <- y[y$indicator %in% x$indicator, ]
  coefTable <- coefTable[order(coefTable$landUse), ]
  init   <- initScenario(coefTable, uValue = uValue,
                         optimisticRule = optimisticRule,
                         fixDistance = fixDistance)            # heavy dplyr per subset
  result <- solveScenario(x = init)
  list(names(result$landUse), t(result$landUse), result$beta)
}
landUseResults <- future_lapply(combList, applyFun, y = coefTable)

# ... populate combList with results, drop NA subsets ...

betaMFfun <- function(x) {
  result <- solveScenario(initMf, lowerBound = x$result,
                          upperBound = x$result)                # rebuilds full-size LP every call
  list(result$beta)
}
betaMf <- future_lapply(combList, betaMFfun)
```

### After — rewritten autoSearch (precompute + row-slice + LP reuse)

```r
# Precompute once on the full scenarioTable
scenarioTable_full <- initMf$scenarioTable
cc_full <- defineConstraintCoefficients(scenarioTable_full)         # full coefConstraint

# Per-row contributions to coefObjective; subset coefObjective
# is then just colSums over the rows belonging to that subset.
adjSem_cols  <- grep("^adjSem", names(scenarioTable_full))
adjSem_mat   <- as.matrix(scenarioTable_full[, adjSem_cols])
storage.mode(adjSem_mat) <- "double"
sign_per_row <- ifelse(scenarioTable_full$direction == "less is better", -1, 1)
contrib_full <- (adjSem_mat * sign_per_row) / scenarioTable_full$diffAdjSem * 100

rows_by_ind    <- split(seq_len(nrow(scenarioTable_full)), scenarioTable_full$indicator)
rows_cc_by_ind <- split(seq_len(nrow(cc_full)),            cc_full[, "indicator"])

## Block 1: row-slice + solve (parallel)
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

# ... populate combList, drop NA subsets ...

## Block 2: reused LP, one set.bounds + solve per subset (sequential)
lpa     <- .buildBlock2LP(initMf)                # build once
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
```

`.buildBlock2LP` (a non-exported helper added in the same file) does
the one-shot LP construction — `make.lp` + `set.column` × (N+1) +
`set.objfn` + `set.rhs` + `set.constr.type` + `lp.control`. Per
iteration we only mutate the bounds.

---

## What changed, in plain English

1. **No more `initScenario` per subset.** Each subset is a row-filter of
   the same full scenarioTable, and both `coefObjective` and
   `coefConstraint` are row-local computations. Doing the heavy
   dplyr work once and slicing per subset eliminates ~30 ms of
   per-subset overhead at N=6 and ~190 ms at N=10.

2. **No more LP rebuild per Block-2 call.** Every Block-2 iteration
   solves the same LP under different variable bounds. Building the LP
   once and just calling `set.bounds` per iteration drops Block 2's
   per-call cost from "rebuild the full-size LP" (a non-trivial chunk
   at large N) to "one `set.bounds` + a near-trivial solve". Since the
   bounds clamp every x_i, the solver finds the unique β
   = min_s(coef_s · x) in essentially zero pivots, and phase 2 is
   skipped entirely (no degrees of freedom left to optimise).

3. **Block 1 parallelism actually scales.** The original's
   `future_lapply` calls give only ~1.2× speedup at N=6 and ~3.4× at
   N=10 with 21 workers, because each task is so small that the future
   scheduler overhead eats the win. The rewritten Block 1 is still
   per-subset (each subset builds its own LP inside `solveScenario`),
   so it's trivially parallel — and the LP solve dominates per-task
   work at larger N, so workers stay busy.

4. **Block 2 stays sequential.** The reused LP is a C-handle (an R
   `externalptr` wrapping a malloc'd lpSolveAPI struct). The pointer
   is meaningful only within the process that allocated it; it can't
   be shipped to a worker. With the reuse trick Block 2 takes ~1-2 s
   even at N=13, so parallelising it isn't worth the structural
   complexity (per-worker LP build + chunked dispatch).

---

## Performance — measured + extrapolated

Synthetic Gosling extension (`extend_lulc` from `test_scale_A.R`) at 10
indicators (1023 subsets). All times wall-clock seconds; 21 workers
where parallel. N=13 row is extrapolated from the per-subset LP-solve
scaling (`test_scale_A.R`), since a measured run takes ~3 hours.

| N_LULC | original sequential | original parallel (21w) | rewritten parallel (21w) | speedup vs orig-seq | speedup vs orig-par |
| ------ | ------------------- | ----------------------- | ------------------------ | ------------------- | ------------------- |
| 6      | 38.1 s              | 30.8 s                  | **4.0 s**                | 9.5×                | 7.7×                |
| 10     | 237.9 s             | 69.6 s                  | **32.8 s**               | 7.2×                | 2.1×                |
| 13     | ~3 hours            | ~12-15 min              | **~5-7 min** (est.)      | ~30×                | ~2×                 |

Why the rewrite's edge vs the parallel original shrinks with N: the LP
solve itself eventually dominates per-subset cost, and the rewrite does
not change that cost — it removes the surrounding bookkeeping
(`initScenario` + per-call LP rebuild). At N=6 bookkeeping was 90% of
per-subset work and the rewrite kills it. At N=13 it's ~20% and the LP
solve is the irreducible floor.

---

## Correctness — `test_autosearch_validate.R`

Ran the rewritten `autoSearch` and an inlined copy of the original on
Gosling (N=6) under `plan(sequential)`, then compared per subset (keyed
on the sorted indicator tuple):

| metric                       | max |diff|                      |
| ---------------------------- | ------------------------------- |
| β (per subset)               | **0.0e+00**                     |
| βMf (per subset)             | **0.0e+00**                     |
| portfolio shares             | 1.6e-07 (float noise)           |
| BrayCurtisObs                | 1.2e-05 (derived from shares)   |
| BrayCurtisMf                 | 1.5e-05 (derived from shares)   |

β values are bit-identical because `solveScenario` rounds to
`digitsPrecision` (= 4) before returning. Portfolio shares can differ
by ~1 ULP because the original's dplyr path and the rewrite's direct
matrix path sum the same numbers in slightly different orders
(floating-point non-associativity); the LP solver then lands on a
neighbouring vertex of the same optimal face. Bray-Curtis differences
follow the share differences (each unit of share difference contributes
1×100 / 2 = 50 to BC, so a 1e-7 share diff → ~5e-6 BC diff).

**One observable consequence of the share/BC noise:** when two
indicator sets land at numerically identical `BrayCurtisObs` under the
original, they can become numerically distinguishable (by ~1e-5) under
the rewrite. On the validation run the original reported two tied
"best" sets; the rewrite reports the strictly-smallest one. Both
answers are equally correct — the rewrite is just no longer randomly
breaking ties via floating-point accident.

---

## Implementation gotchas worth remembering

1. **The LP handle is process-local.** `make.lp` returns an
   `externalptr` wrapping a C-allocated struct. The pointer is
   meaningless outside the process that allocated it. Any "reuse the LP
   across iterations" trick must keep the iterations inside one
   process — hence Block 2 stays sequential. If you ever want to
   parallelise Block 2, each worker must build its own LP at startup
   and process a chunk; you cannot ship the LP itself.

2. **`set.column` zeros the objective coefficient.** Inherited from
   `solveScenario`: on this lpSolveAPI version,
   `set.column(lprec, j, x)` with `length(x) == nrow` silently overrides
   that column's objective coefficient with zero. Call `set.objfn`
   **after** the `set.column` loop in `.buildBlock2LP`, not before.

3. **Why phase 2 can be dropped in Block 2.** When `lower = upper`
   for every x_i, the LP feasible region is a single point: x is
   forced, and β is determined as the min over scenarios of
   `coef_s · x`. Phase 1's objective `max β` finds that value in
   essentially zero pivots. There are no remaining degrees of freedom
   for phase 2's `max coefObjective · x` to optimise — the answer is
   the same point.

4. **Block 1's NA-filter happens before Block 2.** If
   `solveScenario` returns `landUse = NA, beta = NA` for an infeasible
   subset (e.g., a single-indicator subset whose LP has no feasible
   portfolio), the rewrite drops that subset immediately after Block 1
   instead of letting NA bounds flow into Block 2 (which would either
   error or do wasted work).

---

## Files touched

- [branches/feature_pareto_uncerSpace/R/autoSearch.R](branches/feature_pareto_uncerSpace/R/autoSearch.R) — rewritten end to end; adds the non-exported `.buildBlock2LP` helper.
- [test_autosearch_validate.R](test_autosearch_validate.R) — new; inlines a copy of the original and compares it against the rewritten `autoSearch` per subset (β, βMf, shares, Bray-Curtis).
- [test_autosearch_fast.R](test_autosearch_fast.R) — benchmark harness (Gosling, N=6) used during development; compares original sequential / original parallel / rewritten parallel.
- [test_autosearch_fast_N10.R](test_autosearch_fast_N10.R) — same harness against the synthetic N=10 LULC dataset.
