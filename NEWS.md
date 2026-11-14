# r4subpharma (development version)

- `datasetjson_to_evidence()`: emit structural-presence evidence from a CDISC
  Dataset-JSON data-exchange file's metadata - either a parsed Dataset-JSON
  object (a list with a `columns` element, as `jsonlite::fromJSON()` or
  `datasetjson::read_dataset_json()` produce) or a plain data.frame of column
  metadata. It records whether the file declares its dataset-level keys (name,
  label, record count) under the new `Q-DJSON-001` indicator and whether each
  column declares its required keys (name, label, data type) under the new
  `Q-DJSON-002` indicator. Both identifiers are in a dedicated `Q-DJSON` family
  that does not collide with the `Q-DEFINE-*`, `Q-ADAM-*`, or `T-ADAM-*`
  indicators already in use. Added `as_datasetjson_metadata()` to normalize a
  parsed Dataset-JSON object or data.frame into the Dataset-JSON metadata
  contract. This is presence evidence only: it records whether each metadata
  key is populated as the file reports it, and does not judge whether data
  types are correct, whether the record count matches the rows, or whether
  values conform to any controlled terminology. The core transform operates on
  a plain data.frame, so it needs no external package installed. Existing
  adapters are unchanged.
- `codelist_to_evidence()`: emit controlled-terminology documentation-presence
  evidence from a `metacore` object's `value_spec` and `codelist` - for each
  variable that references a codelist, whether that codelist is documented
  (resolves to an enumerated list with at least one term, or an
  external-dictionary reference such as MedDRA/WHODrug) - under the new
  `Q-DEFINE-005` indicator (distinct from the `Q-DEFINE-001` through
  `Q-DEFINE-004` already in use). Added `as_codelist_metadata()` to normalize a
  `metacore` object or data.frame into the codelist-metadata contract. This is
  presence evidence only: it does not judge whether terms are the correct CDISC
  Controlled Terminology values. Existing adapters are unchanged.
- `pharma_evidence()`: a convenience wrapper that composes the existing
  adapters into one combined evidence table in a single call. It runs
  `metacore_to_evidence()` and `dataset_metadata_to_evidence()` and, when an
  ADaM data.frame is supplied, `adam_to_evidence()`, then row-binds the
  results. Pure composition - no new indicators or semantics; the output
  equals binding the individual adapters' outputs. Existing adapters are
  unchanged.
- `dataset_metadata_to_evidence()`: emit dataset-level documentation-presence
  evidence from a `metacore` object's `ds_spec` and `ds_vars` - whether each
  dataset documents a label, a structure, and at least one key variable -
  under the dedicated `Q-DEFINE-004` indicator (distinct from the label-only
  `Q-DEFINE-001`). Added `as_dataset_metadata()` to
  normalize a `metacore` object or data.frame into the dataset-metadata
  contract. Existing adapters are unchanged.

# r4subpharma 0.2.0

- Prepared for CRAN submission; metadata and documentation polish.
- Expanded test coverage: the `metacore` path where `value_spec` omits a
  `dataset` column (recovered from the qualified `DATASET.VARIABLE` key), and
  `submission_readiness()` forwarding an explicit `sci_config`. No change to
  adapter behaviour.

# r4subpharma 0.1.0

- Initial release.
- `as_variable_metadata()`: normalize a `metacore` object or a data.frame into
  the shared variable-metadata contract used by the adapters.
- `metacore_to_evidence()`: convert metadata into R4SUB documentation evidence,
  reusing the `Q-DEFINE-002` and `Q-DEFINE-003` indicators.
- `adam_to_evidence()`: check an ADaM dataset against its metadata and emit
  presence, unexpected-variable, type, and label evidence (`T-ADAM-001`,
  `T-ADAM-002`, `Q-ADAM-001`, `Q-ADAM-002`).
- `submission_readiness()`: harvest evidence from metadata and one or more ADaM
  datasets and compute the Submission Confidence Index in a single call.
- Added the `cdisc-pilot` vignette, a worked case study that scores the CDISC
  pilot ADaM package using public `metacore` and `pharmaverseadam` data.
