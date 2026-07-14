# r4subpharma

<!-- badges: start -->
[![R-CMD-check](https://github.com/R4SUB/r4subpharma/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R4SUB/r4subpharma/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**r4subpharma** connects the [pharmaverse](https://pharmaverse.org) clinical
reporting stack to the **R4SUB** (*Ready for Submission*) ecosystem. It turns the
metadata and datasets a pharmaverse pipeline already produces into standardized
R4SUB evidence, so submission readiness can be scored with
[r4subscore](https://github.com/R4SUB/r4subscore) — **without changing your
pipeline**.

## Why

A pharmaverse workflow already generates the signals that describe how ready a
deliverable is: whether every variable is documented, whether an ADaM dataset
matches its metadata, whether types and labels are in place. Those signals are
usually discarded as console warnings. r4subpharma captures them as evidence and
feeds them to a single, defensible **Submission Confidence Index (SCI)**.

## Installation

```r
# install.packages("pak")
pak::pak("R4SUB/r4subpharma")
```

## The metadata contract

Both adapters work from one small table — one row per dataset variable — that
[`as_variable_metadata()`] builds from either a
[`metacore`](https://github.com/pharmaverse/metacore) object or a plain
data.frame:

| Column | Meaning |
|---|---|
| `dataset`, `variable` | Identify the variable (required) |
| `label`, `type` | Documentation attributes |
| `origin`, `derivation`, `is_derived` | Provenance |

## Quick start

```r
library(r4subpharma)

meta <- data.frame(
  dataset  = "ADSL",
  variable = c("USUBJID", "AGE", "SEX"),
  label    = c("Unique Subject Identifier", "Age", "Sex"),
  type     = c("text", "integer", "text"),
  origin   = c("Predecessor", "Derived", "Predecessor"),
  derivation = c(NA, "Age at informed consent", NA),
  stringsAsFactors = FALSE
)

adsl <- data.frame(USUBJID = "01-001", AGE = 54, SEX = "M")

ctx <- r4subcore::r4sub_run_context("STUDY01", "PROD")
res <- submission_readiness(list(ADSL = adsl), meta, ctx)

res$sci$SCI    # Submission Confidence Index, 0-100
res$sci$band   # decision band
```

In a real pipeline the metadata comes from `metacore` and the datasets from
`admiral`; the call is otherwise identical.

## How evidence maps to the SCI pillars

| Adapter | Indicators | SCI pillar |
|---|---|---|
| `metacore_to_evidence()` | `Q-DEFINE-002`, `Q-DEFINE-003` | quality |
| `adam_to_evidence()` | `T-ADAM-001`, `T-ADAM-002` | trace |
| `adam_to_evidence()` | `Q-ADAM-001` | quality |
| `adam_to_evidence()` | `Q-ADAM-002` | usability |

See `vignette("r4subpharma")` for the full walkthrough.

## Part of the R4SUB ecosystem

- [r4subcore](https://github.com/R4SUB/r4subcore) — evidence schema and parsers
- [r4subscore](https://github.com/R4SUB/r4subscore) — Submission Confidence Index
- [r4sub](https://github.com/R4SUB/r4sub) — meta-package for the ecosystem

## License

MIT © Pawan Rama Mali
