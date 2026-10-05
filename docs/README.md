# Annotated statistical source views

These views add 36 detailed inline review comments across the four central analysis files. They explain time bounds and censoring, risk transformation, AUC ties, Brier normalization, paired bootstrap intervals, training-only preprocessing, synthetic donor matching, tuning, neural likelihood gradients and conditional margin tests.

- [Cohort follow-up and conventional models](hrs_analysis.R.md)
- [IPCW and nested primary validation](hrs_prediction_validation.R.md)
- [Machine learning and synthetic training](hrs_ml_extension.R.md)
- [Conditional margin tests and Holm adjustment](hrs_ml_equivalence.R.md)

Each view names its exact source and SHA-256. Removing only the added REVIEW NOTE lines reproduces the source exactly. `python3 -B audit/verify_annotations.py` verifies this. These are documentation views, not alternate executable source files. Existing source comments and historical snapshots are retained.

The original scripts also contain implementation comments and artificial-data tests. The annotations explain selected core logic in greater depth. They do not imply every line has undergone an independent external audit.
