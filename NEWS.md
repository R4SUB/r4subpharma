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
