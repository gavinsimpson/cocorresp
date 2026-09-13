# Numerical reference provenance

`legacy.R` was generated from cocorresp commit `9d405b7` (version 0.4-6),
on R 4.6.1 on macOS arm64, before implementation changes.

The community matrices use `set.seed(20260913)` followed by 80 exponential
draws for a 16-by-5 response and 96 for a 16-by-6 predictor. References use
two axes; the permutation test uses `set.seed(42)` and nine replicates.

Stored values: symmetric eigenvalues, SIMPLS transformed fitted values,
LOO CV fit, permutation p-values/statistics/inertia/axis fit. These are
compatibility references, not independent evidence of scientific correctness.
Independent tests use explicit matrix calculations and the pls implementation.
