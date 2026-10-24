# Additional handgrip prediction analyses

Code and released aggregate results for Tables S11–S17, the additional sex comparison, descriptive counts and post hoc primary-model diagnostics. These exploratory analyses followed the original results. Primary Table 2 is unchanged.

## Setup

Use the existing corrected base package, R 4.6.0, survival 3.8-6, rpart 4.1.27 and xgboost 3.2.1.1. Python helpers use the standard library. From the repository root:

```sh
python3 -B package/materialize_version.py corrected supplementary_analyses/implementation/analysis-work
```

HRS data require registration and applicable permissions. Execute data-dependent commands locally with authorized access and new private output directories. Participant data, predictions, fitted objects and LOCAL_ONLY outputs are not included.

## Reproduction

From supplementary_analyses/revision, CORRECTED_SCRIPTS denotes ../implementation/analysis-work/02_Analyse/scripts:

```sh
Rscript --vanilla scripts/run_review_private.R DATA_DIR PRIVATE_OUTPUT CORRECTED_SCRIPTS
Rscript --vanilla scripts/release_oof_safely.R COMPLETED_RUN
Rscript --vanilla a1_sex_20261021/export_a1_sex.R DATA_DIR PRIVATE_OUTPUT CORRECTED_SCRIPTS
Rscript --vanilla residual_counts_20261021/export_residual_counts.R DATA_DIR PRIVATE_OUTPUT CORRECTED_SCRIPTS
python3 -B scripts/build_display_tables.py NEW_DISPLAY_DIR
```

The diagnostic export, from the repository root:

```sh
Rscript --vanilla supplementary_analyses/implementation/patches/local_completion119/local_checks.R DATA_DIR PRIVATE_OUTPUT supplementary_analyses/implementation
```

Diagnostics use two fixed primary fits and the same cohort/formula functions, without bootstrap or tuning. No rerun is needed to inspect the supplied aggregates. Their source hashes preserve the original run identities; fresh runs use corrected disclosure code. Only reviewed SUCCESS outputs passing disclosure checks may be shared.

Display CSVs map to S11–S17; diagnostic_results maps to S7. Rounding and SUPPRESSED markers are retained. The separate additional validation uses 200/50 household resamples. Risk-change intervals use 5,000 fixed-prediction draws and omit retraining uncertainty. Complete exploratory fitted parameters are not supplied. Full fresh-environment HRS reproduction is not claimed.

## Artificial-data checks

After setup, from the repository root:

```sh
python3 -B supplementary_analyses/revision/tests/test_a1_sex_display.py
Rscript --vanilla supplementary_analyses/revision/tests/test_oof_disclosure.R supplementary_analyses/revision supplementary_analyses/revision/scripts/hrs_review_oof.R
Rscript --vanilla supplementary_analyses/revision/a1_sex_20261021/test_a1_sex_export.R supplementary_analyses/revision/a1_sex_20261021
Rscript --vanilla supplementary_analyses/implementation/patches/local_completion119/test_local_checks.R supplementary_analyses/implementation/patches/local_completion119
```

OpenAI Codex supported analysis coding, reproducibility checks and documentation. The authors reviewed all AI-assisted content and retain responsibility for the released materials. Code uses the repository MIT license; HRS data conditions remain separate. FILE_SHA256.json records file identities.
