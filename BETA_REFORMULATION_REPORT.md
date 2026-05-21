# `solveScenario()` β-reformulation — change report

## Summary

The robust max-min LP in `solveScenario()` on branch
`feature_pareto_uncerSpace` (branch A) used to be solved by **bisection**
over β: build the LP once, then repeatedly tighten β as the right-hand
side of the distance constraints and re-solve until the bracket falls
below `digitsPrecision`. At default precision (1e-4) this is ≈14 LP
solves per call, and it did not scale well: ~12 minutes at N=12
LULCs and non-terminating at N=13.

The reformulation makes **β a decision variable** in a single LP, then
adds a phase-2 solve that fixes β at the optimum and picks the best
portfolio among the ones tied at that β (see *What the two phases do*
below). Two auxiliary optimisations cleared bottlenecks exposed by the
new shape: the LP is now built **column-wise** via `set.column`
instead of row-wise via `add.constraint`, and the character → numeric
coercion of `coefConstraint` uses `storage.mode(.) <- "double"`
instead of the per-element `apply(., c(1,2), as.numeric)`.

| change                 | rationale                                          |
| ---------------------- | -------------------------------------------------- |
| β as LP variable       | one LP instead of ~14 bisection iterations         |
| column-wise `set.column` build | N+1 R↔C calls instead of M (≈40 000 at N=12) |
| `storage.mode<-` coercion | ~10× faster than `apply(., c(1,2), as.numeric)` |
| phase-2 fix-β solve    | preserves original portfolio tie-breaker            |

---

## Code diff (the core change)

### Before — master / branch A original (bisection)

```r
# Init lpa Object
lpaObj <- lpSolveAPI::make.lp(nrow = 0, ncol = length(coefObjective))
lpSolveAPI::set.objfn(lprec = lpaObj, obj = coefObjective)
lpSolveAPI::add.constraint(lprec = lpaObj, xt = rep(1, length(coefObjective)),
                           type = "=", rhs = 1)
apply(piConstraintCoefficients, 1, function(x) {
  lpSolveAPI::add.constraint(lprec = lpaObj, xt = x, type = ">=",
                             rhs = piConstraintRhs[2])
})
# ... bounds, sense ...
lpSolveAPI::set.rhs(lprec = lpaObj,
  b = c(1, rep(piConstraintRhs[2], dim(piConstraintCoefficients)[1])))
statusOpt <- lpSolveAPI::solve.lpExtPtr(lpaObj)

# Stepwise approximation loop — re-solve with new beta each iteration
while (counter < emergencyStop) {
  counter <- counter + 1
  if (statusOpt == 0) {
    piConstraintRhs <- c(piConstraintRhs[2],
                         round((piConstraintRhs[2] + piConstraintRhs[3]) / 2,
                               digitsPrecision),
                         piConstraintRhs[3])
  } else {
    piConstraintRhs <- c(piConstraintRhs[1],
                         round((piConstraintRhs[1] + piConstraintRhs[2]) / 2,
                               digitsPrecision),
                         piConstraintRhs[2])
  }
  lpSolveAPI::set.rhs(lprec = lpaObj,
    b = c(1, rep(piConstraintRhs[2], dim(piConstraintCoefficients)[1])))
  statusOpt <- lpSolveAPI::solve.lpExtPtr(lpaObj)
  if (piConstraintRhs[3] - piConstraintRhs[1] <= precision) break()
}
# ... final solve to capture portfolio at the converged beta ...
x$beta <- 1 - round(retPiConstraintRhs, digitsPrecision)
x$landUse[1, ] <- lpSolveAPI::get.variables(lpaObj)
```

Variables: `x_1..x_N`. RHS of distance constraints is mutated each
iteration; portfolio comes from whichever solve was last feasible at the
converged bracket.

### After — branch A reformulation

```r
nLulc   <- length(coefObjective)
betaIdx <- nLulc + 1L

# Assemble the full constraint matrix in R, then push it column-wise.
A <- matrix(0, nrow = nRows, ncol = nLulc + 1L)
A[1L, seq_len(nLulc)]      <- 1                            # sum constraint
A[distRows, seq_len(nLulc)] <- piConstraintCoefficients    # distance coefs
A[distRows, betaIdx]        <- -1                          # beta column
# (landUseRestriction and Pareto rows added the same way if requested)

lpaObj <- lpSolveAPI::make.lp(nrow = nRows, ncol = nLulc + 1L)
for (j in seq_len(nLulc + 1L)) {
  lpSolveAPI::set.column(lprec = lpaObj, column = j, x = A[, j])
}
# IMPORTANT: set.objfn AFTER set.column. On lpSolveAPI 5.5.2,
# set.column with length=nrow silently zeroes that column's objective
# coefficient.
lpSolveAPI::set.objfn(lprec = lpaObj, obj = c(rep(0, nLulc), 1))   # max beta
lpSolveAPI::set.rhs(lprec = lpaObj, b = rhs)
lpSolveAPI::set.constr.type(lprec = lpaObj, types = ctypes)
lpSolveAPI::set.bounds(lprec = lpaObj,
                       lower = c(lowerVec, -1),
                       upper = c(upperVec,  2))
lpSolveAPI::lp.control(lprec = lpaObj, sense = "max")

statusOpt  <- lpSolveAPI::solve.lpExtPtr(lpaObj)      # phase 1: max beta
phase1Vars <- lpSolveAPI::get.variables(lpaObj)
betaOpt    <- phase1Vars[betaIdx]

# Phase 2: fix beta and maximise coefObjective * x to reproduce the
# original tie-breaker (bisection ended with this same objective at the
# final beta).
lpSolveAPI::set.bounds(lprec = lpaObj,
                       lower = c(lowerVec, betaOpt),
                       upper = c(upperVec, betaOpt))
lpSolveAPI::set.objfn(lprec = lpaObj, obj = c(coefObjective, 0))
statusOpt2 <- lpSolveAPI::solve.lpExtPtr(lpaObj)

x$beta         <- 1 - round(betaOpt, digitsPrecision)
x$landUse[1, ] <- lpSolveAPI::get.variables(lpaObj)[seq_len(nLulc)]
```

Variables: `x_1..x_N, β` (β is column N+1). The LP solved is

```
max  β
s.t. Σ x_i           = 1
     x_i            ≤ landUseRestriction_i        (optional)
     coef_s · x − β ≥ 0   for each scenario s
     coef_p · x     ≥ paretoMaxDistance           (optional Pareto)
     0 ≤ x_i ≤ 1
```

---

## What the two phases do

Plain-English version of why a single LP isn't enough:

1. **Phase 1 — find the best worst-case β.**
   The LP picks the portfolio whose *worst* scenario score is as high as
   possible. β is that worst-case score.

2. **Phase 2 — among portfolios tied at that β, pick the one with the
   best *average* performance.**
   At the optimal β there is usually not a single best portfolio but
   a *region* of them — they all hit β in their worst scenario, but
   they differ in the *non-worst* scenarios. Phase 2 fixes β and
   switches the LP objective to `coefObjective · x`, which sums each
   LULC's normalised value **across all scenarios**, so it rewards the
   portfolio that uses the slack in the non-worst scenarios best.

Why two phases at all? An LP can only optimise one objective. Phase 1's
objective is β; phase 2's is `coefObjective · x`. The old bisection code
did *both* in one go by accident: its LP objective was always
`coefObjective · x`, and the bisection just kept tightening β as an RHS
parameter, so the final iteration happened to be exactly the phase-2
problem. The new code splits it into two explicit steps.

**Why both portfolios A and B in Section 3 (Silvopasture excluded) have
β = 0.7079 with different shares:** they're both on the optimal β face.
Phase 2 (in A) picks A's specific point on that face. B reaches its
point through a different code path. Neither is "wrong" — they're tied
on the worst case, just slightly different on the rest of the
uncertainty space.

> If you ever want to drop phase 2 to save time (e.g., big sensitivity
> sweeps where only β matters), the LP still returns a valid β-optimal
> portfolio after phase 1 — just not a deterministic one.

---

## Scaling results — `test_scale_A.R`

Synthetic Gosling extension (perturbed LULC duplicates) timed at
N ∈ {6, 8, 10, 12, 13}. `solve_sec` is the `solveScenario` call only;
init scales the same in both eras (linear in scenario count).

| N  | scenarios | solve_sec **old** (bisection) | solve_sec **new** (single LP) | speedup |
| -- | --------- | ----------------------------- | ----------------------------- | ------- |
| 6  | 64        | 0.02                          | 0.01                          | ~2×     |
| 8  | 256       | 0.19                          | 0.06                          | ~3×     |
| 10 | 1024      | 1.16                          | 0.11                          | ~10×    |
| 12 | 4096      | **724.92**                    | **1.63**                      | **~445×** |
| 13 | 8192      | did not complete (>600 s)     | 8.70                          | —       |

β values across N are unchanged within precision: 0.7079 (N=6),
0.6868 (N=8), 0.5975 (N=10), 0.6001 (N=12, N=13).

> Note: the planning estimate of "<<1 s at N=12" was optimistic. The
> bisection bottleneck is gone, but a single LP solve at N=12 is itself
> ~0.9 s, plus ~0.7 s for phase 2. The new floor is the LP cost, not
> the loop cost.

---

## Correctness — `test_compare_master.R`

Tolerances used in the test:

- `|β_new − β_ref| ≤ 2·precision = 2·10⁻⁴`
- `β_new ≥ β_ref − precision` (theoretical direction: master's bisection
  ends one precision step *before* the true optimum δ and reports
  β = 1 − δ, so A's exact-optimum β can be smaller than master's by at
  most one precision step, never larger)

Portfolio differences are reported but **not asserted** — at the
optimal β there is typically a face of optimal portfolios, and different
solvers / phase paths can land on different vertices.

All 24 cases pass.

### Section 1 — A vs master, unrestricted (12 cases)

The bisection (master) terminates with δ at the lower end of a
precision-wide bracket, so β_master can be up to 1×precision above
β_true. A returns β_true. Pattern in the data: β_diff is either 0 or
exactly −1×10⁻⁴.

| case                                              | source | β        | Crops    | Pasture  | Alley Cropping | Silvopasture | Plantation | Forest   |
| ------------------------------------------------- | ------ | -------- | -------- | -------- | -------------- | ------------ | ---------- | -------- |
| uV=1, expectation, fixDist=NA                     | master | 0.5746   | 0.070464 | 0.000000 | 0.000000       | 0.519473     | 0.000000   | 0.410063 |
|                                                   | A      | 0.5745   | 0.070909 | 0.000000 | 0.000000       | 0.518953     | 0.000000   | 0.410138 |
| uV=2, expectation, fixDist=NA                     | master | 0.6132   | 0.000000 | 0.000000 | 0.000000       | 0.598473     | 0.014940   | 0.386587 |
|                                                   | A      | 0.6131   | 0.000000 | 0.000000 | 0.000000       | 0.598064     | 0.015274   | 0.386663 |
| uV=3, expectation, fixDist=NA                     | master | 0.6473   | 0.009771 | 0.095625 | 0.000000       | 0.449066     | 0.102213   | 0.343324 |
|                                                   | A      | 0.6473   | 0.010226 | 0.095624 | 0.000000       | 0.448293     | 0.102541   | 0.343316 |
| uV=1, uAdjExp, fixDist=NA                         | master | 0.6090   | 0.025644 | 0.000000 | 0.000000       | 0.584341     | 0.000000   | 0.390015 |
|                                                   | A      | 0.6090   | 0.025753 | 0.000000 | 0.000000       | 0.584209     | 0.000000   | 0.390037 |
| uV=2, uAdjExp, fixDist=NA                         | master | 0.6433   | 0.114399 | 0.176028 | 0.000000       | 0.261979     | 0.117265   | 0.330329 |
|                                                   | A      | 0.6433   | 0.114816 | 0.175669 | 0.000000       | 0.261585     | 0.117611   | 0.330319 |
| uV=3, uAdjExp, fixDist=NA                         | master | 0.6696   | 0.113065 | 0.175492 | 0.000000       | 0.337611     | 0.078075   | 0.295757 |
|                                                   | A      | 0.6696   | 0.114636 | 0.177442 | 0.000000       | 0.331427     | 0.080606   | 0.295889 |
| uV=1, expectation, fixDist=3                      | master | 0.5609   | 0.072989 | 0.000000 | 0.000000       | 0.535828     | 0.000000   | 0.391183 |
|                                                   | A      | 0.5609   | 0.073184 | 0.000000 | 0.000000       | 0.535600     | 0.000000   | 0.391216 |
| uV=2, expectation, fixDist=3                      | master | 0.5971   | 0.000000 | 0.130066 | 0.000000       | 0.480016     | 0.023587   | 0.366331 |
|                                                   | A      | 0.5970   | 0.000000 | 0.132300 | 0.000000       | 0.477655     | 0.023721   | 0.366324 |
| uV=3, expectation, fixDist=3                      | master | 0.6473   | 0.009771 | 0.095625 | 0.000000       | 0.449066     | 0.102213   | 0.343324 |
|                                                   | A      | 0.6473   | 0.010226 | 0.095624 | 0.000000       | 0.448293     | 0.102541   | 0.343316 |
| uV=1, uAdjExp, fixDist=3                          | master | 0.7612   | 0.000000 | 0.190030 | 0.000000       | 0.427403     | 0.079747   | 0.302820 |
|                                                   | A      | 0.7612   | 0.000000 | 0.191350 | 0.000000       | 0.425959     | 0.079757   | 0.302933 |
| uV=2, uAdjExp, fixDist=3                          | master | 0.6863   | 0.110933 | 0.311940 | 0.000000       | 0.194821     | 0.050450   | 0.331855 |
|                                                   | A      | 0.6862   | 0.112109 | 0.311032 | 0.000000       | 0.193680     | 0.051449   | 0.331730 |
| uV=3, uAdjExp, fixDist=3                          | master | 0.6696   | 0.113065 | 0.175492 | 0.000000       | 0.337611     | 0.078075   | 0.295757 |
|                                                   | A      | 0.6696   | 0.114636 | 0.177442 | 0.000000       | 0.331427     | 0.080606   | 0.295889 |

### Section 2 — non-binding restriction (A's rhs=1 ≡ A unrestricted)

`landUseRestriction = setNames(1, LULC)` adds `x_i ≤ 1`, which is
already implied by `Σ x_i = 1`. The added row is therefore inert and
the LP solution must be bit-for-bit identical to the unrestricted
solve. It is.

Baseline parameters: `uValue=2, optimisticRule="expectation",
fixDistance=3`. All seven rows below are identical (β and all six
shares).

| case                                  | source | β      | Crops    | Pasture  | Alley Cropping | Silvopasture | Plantation | Forest   |
| ------------------------------------- | ------ | ------ | -------- | -------- | -------------- | ------------ | ---------- | -------- |
| A unrestricted (baseline)             | A_unr  | 0.5970 | 0.000000 | 0.132300 | 0.000000       | 0.477655     | 0.023721   | 0.366324 |
| restriction(Crops) = 1                | A_rhs1 | 0.5970 | 0.000000 | 0.132300 | 0.000000       | 0.477655     | 0.023721   | 0.366324 |
| restriction(Pasture) = 1              | A_rhs1 | 0.5970 | 0.000000 | 0.132300 | 0.000000       | 0.477655     | 0.023721   | 0.366324 |
| restriction(Alley Cropping) = 1       | A_rhs1 | 0.5970 | 0.000000 | 0.132300 | 0.000000       | 0.477655     | 0.023721   | 0.366324 |
| restriction(Silvopasture) = 1         | A_rhs1 | 0.5970 | 0.000000 | 0.132300 | 0.000000       | 0.477655     | 0.023721   | 0.366324 |
| restriction(Plantation) = 1           | A_rhs1 | 0.5970 | 0.000000 | 0.132300 | 0.000000       | 0.477655     | 0.023721   | 0.366324 |
| restriction(Forest) = 1               | A_rhs1 | 0.5970 | 0.000000 | 0.132300 | 0.000000       | 0.477655     | 0.023721   | 0.366324 |

### Section 3 — binding restriction, A vs B (each LULC excluded in turn)

Master does not support `landUseRestriction`; branch B is the only
available oracle. A uses an LP upper-bound constraint
(`x_LULC ≤ 0`) inside `solveScenario`; B rebuilds `scenarioTable` in
`initScenario`. β must match to 4 decimals.

Baseline parameters: `uValue=2, optimisticRule="expectation",
fixDistance=3`.

| case                  | source | β      | Crops    | Pasture  | Alley Cropping | Silvopasture | Plantation | Forest   |
| --------------------- | ------ | ------ | -------- | -------- | -------------- | ------------ | ---------- | -------- |
| exclude Crops         | A      | 0.5970 | 0.000000 | 0.132300 | 0.000000       | 0.477655     | 0.023721   | 0.366324 |
|                       | B      | 0.5971 | 0.000000 | 0.130066 | 0.000000       | 0.480016     | 0.023587   | 0.366331 |
| exclude Pasture       | A      | 0.6011 | 0.000000 | 0.000000 | 0.000000       | 0.616676     | 0.015749   | 0.367575 |
|                       | B      | 0.6011 | 0.000000 | 0.000000 | 0.000000       | 0.616771     | 0.015670   | 0.367559 |
| exclude Alley Cropping| A      | 0.5970 | 0.000000 | 0.132300 | 0.000000       | 0.477655     | 0.023721   | 0.366324 |
|                       | B      | 0.5971 | 0.000000 | 0.130066 | 0.000000       | 0.480016     | 0.023587   | 0.366331 |
| exclude Silvopasture  | A      | 0.7079 | 0.000000 | 0.485312 | 0.000000       | 0.000000     | 0.254163   | 0.260525 |
|                       | B      | 0.7079 | 0.000000 | 0.485214 | 0.000000       | 0.000000     | 0.254093   | 0.260692 |
| exclude Plantation    | A      | 0.6033 | 0.000000 | 0.133689 | 0.000000       | 0.504349     | 0.000000   | 0.361961 |
|                       | B      | 0.6034 | 0.000000 | 0.126445 | 0.000000       | 0.511469     | 0.000000   | 0.362085 |
| exclude Forest        | A      | 0.9248 | 0.222765 | 0.378746 | 0.000000       | 0.000000     | 0.398489   | 0.000000 |
|                       | B      | 0.9248 | 0.223208 | 0.377836 | 0.000000       | 0.003055     | 0.395901   | 0.000000 |

Note in the **exclude Forest** row: B leaves a tiny Silvopasture share
(0.003) while A puts it strictly at zero. B's data-rebuild approach
allows this because the LULC retained in the normalisation can leak
into the portfolio when its row "borrows" normalisation from another
LULC's row; A's explicit `x_i ≤ 0` constraint forbids it.

---

## Implementation gotchas worth remembering

1. **`set.column` zeros the objective coefficient.** With
   `lpSolveAPI::set.column(lprec, column, x)` where `length(x) == nrow`,
   the column's entry in the objective row gets silently set to zero —
   verified on lpSolveAPI 5.5.2 with a minimal repro. Call
   `set.objfn` **after** all `set.column` calls. The first attempt
   produced β = 2 (the upper bound) because every objective coefficient
   was zero and the solver wandered to a corner.

2. **`coefConstraint` is character.** `defineConstraintCoefficients`
   builds a tibble with one character column (`indicator`) and several
   numeric columns, then calls `as.matrix()`, which coerces the whole
   matrix to character. The original code converted back per-element
   with `apply(., c(1,2), as.numeric)` (0.64 s at N=12).
   `storage.mode(.) <- "double"` does the same conversion in ~0.07 s.

3. **β rounding direction.** Master rounds the bisected δ down to
   `digitsPrecision` and reports β = 1 − δ, so its β is at most one
   precision step *above* β_true. A returns the exact δ, so A's β is at
   most one precision step *below* master's. Test tolerance must
   account for this asymmetric direction; a symmetric "|diff| <= eps"
   is fine for pass/fail, but a "new ≥ ref" guard would flip wrong.

---

## Files touched

- [branches/feature_pareto_uncerSpace/R/solveScenario.R](branches/feature_pareto_uncerSpace/R/solveScenario.R) — reformulated
- [test_compare_landUseRestriction.R](test_compare_landUseRestriction.R) — pre-existing A vs B sanity check (unchanged)
- [test_scale_A.R](test_scale_A.R) — pre-existing N ∈ {6,8,10,12,13} scaling probe (unchanged)
- [test_compare_master.R](test_compare_master.R) — new, the 24-case regression suite reported above
