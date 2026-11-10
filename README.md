# r4subpharma

<!-- badges: start -->
[![R-CMD-check](https://github.com/R4SUB/r4subpharma/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R4SUB/r4subpharma/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/r4subpharma)](https://CRAN.R-project.org/package=r4subpharma)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/r4subpharma)](https://CRAN.R-project.org/package=r4subpharma)
<!-- badges: end -->

**r4subpharma** connects the [pharmaverse](https://pharmaverse.org) clinical
reporting stack to the **R4SUB** (*Ready for Submission*) ecosystem. It turns the
metadata and datasets a pharmaverse pipeline already produces into standardized
R4SUB evidence, so submission readiness can be scored with
[r4subscore](https://github.com/R4SUB/r4subscore), **without changing your
pipeline**.

## Why

A pharmaverse workflow already generates the signals that describe how ready a
deliverable is: whether every variable is documented, whether an ADaM dataset
matches its metadata, whether types and labels are in place. Those signals are
usually discarded as console warnings. r4subpharma captures them as evidence and
feeds them to a single, defensible **Submission Confidence Index (SCI)**.

## Installation

```r
install.packages("r4subpharma")
```

Development version:

```r
pak::pak("R4SUB/r4subpharma")
```

## The metadata contract

Both adapters work from one small table, one row per dataset variable, that
[`as_variable_metadata()`] builds from either a
[`metacore`](https://github.com/atorus-research/metacore) object or a plain
data.frame:

| Column | Meaning |
|---|---|
| `dataset`, `variable` | Identify the variable (required) |
| `label`, `type` | Documentation attributes |
| `origin`, `derivation`, `is_derived` | Provenance |

### Dataset-level metadata

`dataset_metadata_to_evidence()` works one level up, from a table with one row
per *dataset* that [`as_dataset_metadata()`] builds from a `metacore` object's
`ds_spec` and `ds_vars` (or a plain data.frame):

| Column | Meaning |
|---|---|
| `dataset` | Identify the dataset (required) |
| `label`, `structure` | Documentation attributes |
| `n_keys` | Number of key variables |

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

## One call for all the evidence

`pharma_evidence()` composes the adapters into a single evidence table. It runs
`metacore_to_evidence()` and `dataset_metadata_to_evidence()`, adds
`adam_to_evidence()` when an ADaM data.frame is supplied, and row-binds the
results. It is pure composition - no new indicators or semantics - so the output
equals binding the individual adapters' outputs yourself:

```r
ev <- pharma_evidence(meta, ctx)                               # metadata only
ev <- pharma_evidence(meta, ctx, data = adsl, dataset_name = "ADSL")
```

## How evidence maps to the SCI pillars

| Adapter | Indicators | SCI pillar |
|---|---|---|
| `metacore_to_evidence()` | `Q-DEFINE-002`, `Q-DEFINE-003` | quality |
| `dataset_metadata_to_evidence()` | `Q-DEFINE-004` | quality |
| `adam_to_evidence()` | `T-ADAM-001`, `T-ADAM-002` | trace |
| `adam_to_evidence()` | `Q-ADAM-001` | quality |
| `adam_to_evidence()` | `Q-ADAM-002` | usability |

See `vignette("r4subpharma")` for the full walkthrough.

## Part of the R4SUB ecosystem

- [r4subcore](https://github.com/R4SUB/r4subcore): evidence schema and parsers
- [r4subscore](https://github.com/R4SUB/r4subscore): Submission Confidence Index
- [r4sub](https://github.com/R4SUB/r4sub): meta-package for the ecosystem

## Maintained by

R4SUB is part of the open-source work of [TechWorksLab](https://techworkslab.com) - clinical programming and regulatory submissions. Maintainer: Pawan Rama Mali.

## License

MIT © Pawan Rama Mali

## Further reading

- [Building ADaM datasets with the admiral package](https://techworkslab.com/insights-adam-admiral), part of the pharmaverse stack this package adapts, from TECHWORKSLAB.
