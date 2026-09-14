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

`matlab.R` transcribes deterministic results printed to four decimal places in
the original MATLAB example transcripts supplied with Cajo ter Braak's source:

- `example_CoCa_trans.txt` (SHA-256
  `1413899d0d956cddd44bf71d12f8386fbcd7305bcd5dcef1073736f44462e19c`)
- `example_PredCoCa.txt` (SHA-256
  `8e3a353da39f119d9fc672eb4d22c9942178a546bd54dcc726b1ccdf33f934df`)
- `example_PredCoCa_eig.txt` (SHA-256
  `e02ae039960847ddbd960805b7cda0e7a8a1d7c86b64e4147ecc0f27e1f7ef50`)
- `example_SymCoCa.txt` (SHA-256
  `c989e38139812688760a66eebbd8be56b232fd4732069e38fd468716e391f556`)
- `example_test_axes_CoCa.txt` (SHA-256
  `9d58371544451938181e3d114ddd9387fff1bc5a467aa09a9d404d7b8dcf6cf5`)

Tests compare at the four-decimal precision available in those transcripts.
They cover symmetric eigenvalues and inertia, the transition-formula first axis,
predictive fitted values and cross-validation, predictive eigenvalues, and the
deterministic parts of the axis tests. MATLAB permutation p-values are excluded
because the transcript records neither the RNG state nor permutation matrices.
