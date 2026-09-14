# Release checklist

- [ ] Review NEWS and intentional numerical/presentation changes.
- [ ] Run all tests, including optional workers; confirm >=95% coverage.
- [ ] Regenerate Markdown roxygen; verify a clean Rd/NAMESPACE diff afterward.
- [ ] Run full `devtools::check()` with examples, vignettes, and PDF manual.
- [ ] Trigger the quality workflow manually for its full release check.
- [ ] Confirm Windows/macOS/Linux, oldrel/devel, and R 4.1 CI results.
- [ ] Run goodpractice, spelling, URL checks, and reverse-dependency checks;
      record justified exceptions in COMPLIANCE.md.
- [ ] Inspect pkgdown pages, figures, internal links, and deployment artifact.
- [ ] After merging, verify the live GitHub Pages deployment from `docs`.
- [ ] Once that deployment is verified, remove legacy tracked `docs/` from the
      source branch and add `docs/` to .gitignore. Do not remove the published site.
- [ ] Set release version, update citation year/version, tag, and archive on Zenodo.
      Record the resulting DOI without inventing one.
- [ ] Submit to CRAN when ready; preserve prior release tags for reproducibility.

## Future JOSS paper

Collect concrete research-use evidence, comparisons with related software,
benchmarks, design rationale, and independently validated examples. Preserve
original Matlab/pls provenance. Record human review of AI-assisted outputs.
Before drafting or submitting, re-read the current JOSS requirements:
https://joss.readthedocs.io/en/latest/submitting.html and
https://joss.readthedocs.io/en/latest/paper.html.

The modernisation does not establish Matlab equivalence, independent research
impact, authorship agreement, or publication eligibility on its own. The human
authors decide these matters and conduct editorial/reviewer conversations.
