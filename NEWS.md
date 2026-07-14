# r4subpharma 0.0.0.9000

- Initial development version.
- `as_variable_metadata()`: normalize a `metacore` object or a data.frame into
  the shared variable-metadata contract used by the adapters.
- `metacore_to_evidence()`: convert metadata into R4SUB documentation evidence,
  reusing the `Q-DEFINE-002` and `Q-DEFINE-003` indicators.
- `adam_to_evidence()`: check an ADaM dataset against its metadata and emit
  presence, unexpected-variable, type, and label evidence (`T-ADAM-001`,
  `T-ADAM-002`, `Q-ADAM-001`, `Q-ADAM-002`).
- `submission_readiness()`: harvest evidence from metadata and one or more ADaM
  datasets and compute the Submission Confidence Index in a single call.
