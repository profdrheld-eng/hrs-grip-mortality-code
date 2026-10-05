# HRS mortality prediction: verified correction package

Prepared for private repository review on 5 October 2026. The author completed the primary, ML and diagnostic IPCW correction runs. Their released aggregate outputs were verified and integrated in the manuscript. This package contains code, artificial-data tests, variable metadata and explicitly selected aggregate reference tables. It contains no HRS microdata, participant-level predictions, fitted model objects, identifier crosswalks or participant-level figures. The author selected MIT licensing and no separate code DOI. Public reviewer access remains pending completion of the repository review.

## Choose the code version first

| Version | Purpose | Result status |
|---|---|---|
| `reported_version/` | Inspect the historical procedures underlying the superseded pre-correction results. The two IPCW files and two Python table builders preserve checked snapshots from before their respective corrections. | Known IPCW and Python input-validation defects retained for provenance. Use the corrected version for supported execution. Full HRS reproduction was not performed during packaging. |
| `corrected_version/overlay/` | Overlay corrected analysis functions, runtime validation, recovery entry points, historical replay sources and regression tests onto a copy of `reported_version/`. | Corrected implementation supporting the updated manuscript. The author-run correction outputs passed the documented aggregate checks. This is the recommended version. |

The correction changes the risk set used at an early censoring time when an event shares that time. Events at that time leave before censoring is evaluated. The R change is in `corrected_version/ipcw_correction.patch`. Separate Python validation changes are in `corrected_version/runtime_validation.patch`. They reject invalid input even with optimized Python and leave verified valid synthetic table outputs unchanged. Cohort rules, outcome definitions, prediction fitting, grids and random seeds are otherwise unchanged between these two bundled versions. See `VERSION_STATUS.md` for provenance and limits.

The local impact audit confirmed changed weights in all 72 checked combinations. Subsequent author-run primary, ML and diagnostic refreshes were completed and their released aggregates checked. The primary AUC/Brier estimates and confidence intervals remained unchanged at five decimal places. The main conclusions were retained. VERSION_STATUS.md describes the verification boundaries.

Historical metric fixtures remain deliberately unchanged for replay validation. Do not combine historical metrics with corrected code and label them a corrected analysis. Recovery scripts check separate historical source identities and stop on mismatches. Do not weaken these guards. No recovery run is needed merely to review or test this repository.

## Contents and integrity

- `reported_version/02_Analyse/scripts/`: 21 R scripts and one Python table helper, covering imports, cohort construction, conventional models, diagnostics, associations, validation, ML, augmentation, simulations and figures.
- `reported_version/02_Analyse/tests/`: 20 existing artificial-data tests.
- `reported_version/01_Aktuelles_Manuskript/manuskript_v2/`: two final figure scripts, a portable copy of the aggregate table builder, historical `table2_source.csv` and `tableS7_tuning.csv` comparison fixtures, and reported runtime versions.
- `reported_version/04_Archiv/Berichte/ml_critical_audit_v1/inference_verification.json`: an aggregate-only historical arithmetic check used as a provenance input by the table builder.
- `corrected_version/`: analytical and runtime-validation corrections, primary/ML/diagnostic recovery scripts, role-specific historical replay sources, aggregate reference fixtures and regression tests.
- `SOURCE_MANIFEST.json`: exact source and bundled hashes, with the single documented portability transformation.
- `FILE_SHA256.json`: allowlisted package-content hashes. The ZIP has a separate SHA-256 file.
- `qa/`: checks performed on this code bundle. No real-data reanalysis is claimed.

From the package root:

```sh
python3 -B verify_local_package.py
```

The verifier checks expected content, modified/missing/extra files, symlinks and source-manifest coverage in normal and optimized Python. Materialization verifies integrity before copying. These checks require a trusted, unchanged local package. Hashes establish content consistency, not the authenticity of a package whose files and manifests have both been replaced. It does not certify confidentiality by itself; content selection was restricted before archiving. Keep future HRS outputs outside this package.

## Runtime and dependencies

The reported analysis used R 4.6.0, survival 3.8-6, rpart 4.1.27 and xgboost 3.2.1.1. `environment/DEPENDENCIES.tsv` distinguishes reported versions from the current local transitive-dependency inventory. Base R supplies the neural-network implementation; no deep-learning framework is required. Python table and packaging helpers use only the standard library. The final package was checked with Python 3.12.14, including normal execution, `-O` and `PYTHONOPTIMIZE=1` for runtime validation. Earlier package-check records are historical. No exact historical analysis Python version is claimed.

XGBoost depends on Matrix, data.table and jsonlite; survival additionally uses Matrix and lattice. Source installation of XGBoost requires GNU make and a C++17 compiler. Some PNG exports require Cairo. Diagnostics call `/usr/bin/shasum`; that system path is retained in the original code. The tested environment is macOS. The test runner requires POSIX process groups. Windows and Linux portability of the full workflow has not been demonstrated. No compiler, runtime or package binaries are included or installed automatically.

Provision the listed versions under your institution's permitted process, then run:

```sh
Rscript --vanilla environment/check_environment.R
```

If your packages are in a separate R library, supply that directory through `R_LIBS_USER` before running R, or pass `--r-library /path/to/R-library` to the synthetic test runner. A project-local library is not included. The environment check returns 1 for missing required packages and 2 for a version mismatch. A dependency inventory is not a lockfile or proof that a fresh machine reproduces the historical run.

## Synthetic checks, no HRS files

```sh
python3 -B run_synthetic_tests.py --version reported --suite quick --report /path/to/new/reported-quick.json
python3 -B run_synthetic_tests.py --version corrected --suite quick --report /path/to/new/corrected-quick.json
python3 -B run_synthetic_tests.py --version corrected --suite regression --report /path/to/new/corrected-ipcw.json
```

The quick suite covers parsers, identifiers created inside the test, missing/special codes, cohort import logic, source hierarchy, count suppression and synthetic margin calculations. The regression suite checks corrected weights, independent survival-package references and the impact-audit helper without fitting prediction models. Every test runs in a disposable copied code tree. The runner accepts no HRS data-directory argument. It requires a new report outside the package and R library, records incomplete/failed runs explicitly, rejects empty suites and stops nested processes on timeout or interruption. The historical fixtures contain invented identifiers, never HRS records.

`--suite all` includes all available tests and fits artificial-data survival/neural/XGBoost models, nested validation and artificial simulations. It can take substantially longer. Four figure-test preview paths now use unique R temporary locations. This test-only change is shared by both versions. The full corrected suite was executed with artificial data, see `qa/corrected_all.json`. Historical R CLI fixtures do not support all absolute script paths containing spaces. Run the documented relative commands from the materialized root and use a temporary directory without whitespace. The runner rejects an unsupported temporary path before starting tests. Test success does not reproduce real-data estimates or establish the coverage of the full model-development procedure.

## Package-tool regression checks

From the package root, with artificial temporary files only:

```sh
python3 -B test_package_integrity.py .
python3 -B test_release_cli.py
```

These tests exercise manifest tampering, optimized Python, missing inputs, symlink rejection, report preservation and process cleanup. They are separate from the R analysis tests. `qa/release_audit.json` records the tested code hashes and limits. `build_local_package.py` is an author-maintenance tool that requires the original source checkout and legacy snapshots. Reviewers do not need to run it.

## Make an isolated runnable copy

Use a new directory outside this package:

```sh
python3 -B materialize_version.py reported /path/to/new/reported-code
python3 -B materialize_version.py corrected /path/to/new/corrected-code
```

The materializer refuses an existing destination. The corrected tree is the complete reported tree plus the allowlisted overlay files. From its root, `python3 -B 02_Analyse/tests/test_python_table_validation.py` checks invented valid and invalid aggregate inputs in normal and optimized Python. This Python test does not access HRS data.

Commands below assume the current directory is the materialized code root. Run actual-data commands manually in an authorized local R session after obtaining the required HRS releases. Do not run them through an AI system with access to HRS microdata.

## Historical impact-audit entry point

The corrected tree also contains `hrs_ipcw_impact_audit.R`. It reconstructs the approved cohort, existing timing/source scenarios and original outer folds, then compares legacy and corrected weights without fitting models. It reads the mirrored legacy files and bundled rounded fold-count reference automatically. In a manually run, private R session:

```sh
Rscript 02_Analyse/scripts/hrs_ipcw_impact_audit.R /path/to/private/data /path/to/new/private/output
```

An optional third argument is the user's existing local primary-parameter RDS. That permits apparent-metric reweighting with frozen parameters after reproducing the legacy apparent values. The RDS is not bundled and must stay private. Review only manually cleared `SEND_BACK` aggregates; `LOCAL_ONLY` remains private. Identical checked weights do not certify every historical bootstrap or simulation. Changed weights do not by themselves quantify a change in the scientific conclusion. Optional apparent reweighting does not update optimism correction, confidence intervals or ML estimates.

## Private data preparation and workflow

Obtain HRS 2010 Core Final Release Version 6.0, 2014 Core Final Release Version 2.0, and 2022 Tracker Version 1 through the [HRS portal](https://hrsdata.isr.umich.edu/data-products), under your own registration and applicable permissions. `DATA_ACCESS.md` specifies input files and access boundaries. None are bundled.

Set two task-specific paths to a private local data directory and a **new** private output directory. Fixed output names in historical scripts can replace earlier results, so use a different output parent for each version and rerun. Do not point them to historical result directories.

```sh
hrs_data_dir=/path/to/private/data
hrs_output_dir=/path/to/new/private/results

# Import checks, no prediction models.
Rscript 02_Analyse/scripts/hrs_feasibility.R "$hrs_data_dir" "$hrs_output_dir"
Rscript 02_Analyse/scripts/hrs_stage2.R "$hrs_data_dir" "$hrs_output_dir"

# Primary prediction validation and stated sensitivity scenarios.
Rscript 02_Analyse/scripts/hrs_prediction_validation.R "$hrs_data_dir" "$hrs_output_dir" 200 50

# Separate conditional association analyses.
Rscript 02_Analyse/scripts/hrs_grip_change.R "$hrs_data_dir" "$hrs_output_dir" 500
Rscript 02_Analyse/scripts/hrs_grip_history.R "$hrs_data_dir" "$hrs_output_dir" 500

# Diagnostics before Table 1, which checks the diagnostics cohort flow.
Rscript 02_Analyse/scripts/hrs_diagnostics.R "$hrs_data_dir" "$hrs_output_dir"
Rscript 02_Analyse/scripts/hrs_table1.R "$hrs_data_dir" "$hrs_output_dir"

# Exploratory original ML, learning curves, augmented associations and simulations.
Rscript 02_Analyse/scripts/hrs_ml_extension.R all "$hrs_data_dir" "$hrs_output_dir"

# Full-fraction 5,000-draw inference and exploratory boundary-test reports.
Rscript 02_Analyse/scripts/hrs_ml_inference.R "$hrs_data_dir" "$hrs_output_dir"

# Primary interval-containment analysis on saved aggregate metrics only.
Rscript 02_Analyse/scripts/hrs_equivalence_sensitivity.R "$hrs_output_dir/validation_v2" "$hrs_output_dir/primary_margins"
```

The scripts record `STATUS.txt` and return timestamped ML directories. Select only completed output directories from the same code version. Primary validation uses 200 outer/50 inner household resamples; associations use 500 refits. The full ML profile uses five outer/three inner folds, learning fractions 0.5/0.75/1, 500 conditional draws and 100 repetitions per simulation scenario; the inference profile uses full outer-training fractions and 5,000 draws. Settings and seeds are defined in the code. No elapsed-time estimate or successful HRS run is implied by these commands.

The descriptive reporting/primary-parameter reconstruction helper is included as `hrs_reporting_patch.R`. It uses the two bundled historical aggregate tables as reference checks. It also creates exact counts and model specifications under `LOCAL_ONLY`, which must remain private. Use it for the reported version only after verifying that comparison with those references is intended. It is not the corrected-result update workflow.

## Figures and aggregate table assembly

`hrs_paper_figures.R` recreates conventional figures after checking the saved diagnostics, validation and association results under the output parent. It fits the original models and produces some participant-level visualizations locally. `hrs_ml_person_figures_local.R` recreates predictions with saved configurations and tuning from a completed inference directory, checks code hashes/versions/metrics, and produces local participant-level PNGs. These outputs are not automatically shareable.

```sh
Rscript 02_Analyse/scripts/hrs_paper_figures.R "$hrs_data_dir" "$hrs_output_dir"
Rscript 02_Analyse/scripts/hrs_ml_person_figures_local.R "$hrs_data_dir" /path/to/completed/inference "$hrs_output_dir"
Rscript 02_Analyse/scripts/hrs_ml_figures.R /path/to/completed/full-ml /path/to/completed/inference /path/to/new/aggregate-figures split
Rscript 01_Aktuelles_Manuskript/manuskript_v2/build_figure3.R /path/to/completed/inference /path/to/new/figure3
```

`build_figure_s1.R` preserves the already reported exact aggregate flow counts and recreates the current Figure S1. It does not infer a new cohort or validate that those counts still apply. Figure selection, captions and the final journal document are editorial steps beyond this analytical bundle.

The current table builder accepts five explicit locations, with a new output directory:

```sh
python3 01_Aktuelles_Manuskript/manuskript_v2/build_package.py /path/to/old-aggregate-inputs /path/to/completed/inference /path/to/completed/full-ml /path/to/new/table-output 04_Archiv/Berichte/ml_critical_audit_v1/inference_verification.json
```

Both versions replace fixed path configuration and the archived verification-file input with CLI arguments. The corrected version additionally enforces runtime validation regardless of Python optimization and rejects invalid contrast values. Valid synthetic output tables were compared byte for byte with the preserved legacy builders. A failed table build can leave partial files in its new output directory. Use only a successfully completed build whose validation output you have checked. The original input naming is preserved: `table1.csv`, `prognosekennzahlen.csv` (primary `validated_metrics.csv`), `veraenderung_kurve.csv` (change `risk_curve.csv`), `vorgeschichte_kurve.csv` (history `risk_curve.csv`), and `grenzenraster.csv` (primary `margin_sensitivity.csv`). The `inference` and `full-ml` input names are those created by the ML scripts. The bundled prior verification JSON documents historical arithmetic only; it does not validate corrected outputs. This builder contains historical captions and does not recreate every later editorial adjustment in the final manuscript. Compare generated table cells with the accepted tables before replacing any document.

## Author decisions still required

The author selected a public GitHub release identified by tag and full commit hash, MIT licensing and no separate code DOI. Public access must still be verified once enabled. A local ZIP alone does not settle the [BMC software-and-code requirements](https://link.springer.com/brands/bmc/editorial-policies). The author team will supply final author metadata and submission declarations later. Corrected aggregate verification is complete. Public access verification and final manuscript author metadata remain open. The code audit cannot guarantee that no defect or reviewer criticism remains.

## Recovery entry points

After materializing corrected_version, run from the materialized project root. Private input paths must be supplied by the authorized user.

```sh
Rscript 02_Analyse/scripts/hrs_ipcw_primary_refresh.R DATA_DIR OUTPUT_PARENT AUDIT_SEND_BACK
Rscript 02_Analyse/scripts/hrs_ipcw_ml_refresh.R DATA_DIR OUTPUT_PARENT HISTORICAL_FULL_DIR HISTORICAL_INFERENCE_DIR AUDIT_SEND_BACK
Rscript 02_Analyse/scripts/hrs_ipcw_diagnostics_refresh.R DATA_DIR OUTPUT_PARENT HISTORICAL_DIAGNOSTICS_DIR AUDIT_SEND_BACK
```

The recovery tools verify the exact historical sources and versions. They intentionally stop on mismatches. Share only their SEND_BACK aggregates. These entry points do not provide a generic replacement for the original model-development pipeline.
