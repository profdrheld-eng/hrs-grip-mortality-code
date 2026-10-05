# Version and verification status

Local review package, updated 5 October 2026. No publication or submission has occurred.

reported_version preserves the earlier implementation and its known IPCW defect for comparison. It is not the implementation supporting the corrected manuscript. corrected_version overlays the corrected weighting functions, selected-model recovery hooks, primary/ML/diagnostic recovery entry points and tests. Materialize the corrected version for execution.

The author completed all three correction runs. Released aggregates passed source checks, primary validation checks, historical ML reproduction within 1e-8, recomputation of conditional inference and conventional diagnostic invariants. No new hyperparameter search, association analysis or simulation was required. This is verification of the returned aggregates and run metadata, not an independent second fit of the private HRS data.

The recovery scripts require the author's historical run directories and approved HRS data. These private inputs are not bundled. The included audit-reference fixture contains only released aggregate primary metrics and source/version metadata. Historical full and inference sources are separately retained, since they differ in inference-mode and bootstrap-export support.

The package is not a historical lockfile or proof of cross-platform reproducibility. Tests use synthetic data. Conditional bootstrap intervals still omit full retraining uncertainty. No claim of absolute error freedom, clinical equivalence or universal model superiority is made.

Author metadata, final author approval, reviewer access route, archival identifier and any reuse license remain author decisions. No GitHub repository or public archive was created.

All 26 R test scripts passed in a freshly materialized corrected package on 5 October 2026. See qa/corrected_all.json. These synthetic tests complement, but do not replace, the real-data aggregate checks.
