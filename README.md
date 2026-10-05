# Handgrip history and mortality prediction

Analysis code accompanying **Does Prior Handgrip Strength Improve Mortality Prediction Beyond Current Strength in Adults Aged 50 Years or Older? A Comparison of Conventional and Machine Learning Models**.

**Status: public research code, 5 October 2026.** The repository can be accessed without an invitation. The author selected the MIT license and no separate code DOI. The release is identified by its Git tag and full commit hash. This repository contains the sanitized code package, not the manuscript workspace or its Git history. It is not a clinical prediction service.

## Start here

Use the **corrected** implementation. The historical implementation is retained only for provenance and replay checks and contains known corrected defects. The author-run primary, ML and diagnostic correction outputs were verified and integrated in the manuscript. See [version status](package/VERSION_STATUS.md) and [detailed workflow](package/README.md).

From this repository root, with Python and R already installed:

```sh
python3 -B package/verify_local_package.py
Rscript --vanilla package/environment/check_environment.R
python3 -B package/test_package_integrity.py package
python3 -B package/test_release_cli.py
python3 -B package/run_synthetic_tests.py --version corrected --suite all --report ../hrs-synthetic-check.json
python3 -B package/materialize_version.py corrected ../hrs-analysis-work
```

The report file and work directory must be new. If R packages are in a separate library, set R_LIBS_USER for the environment check and pass --r-library /path/to/library to the test runner. Run the [Python table-validation test](package/corrected_version/overlay/02_Analyse/tests/test_python_table_validation.py) from the materialized tree using `python3 -B 02_Analyse/tests/test_python_table_validation.py`.

The test runner creates disposable artificial data. It does not accept HRS input paths. Materialization copies code and performs no analysis. Execute HRS analysis yourself in a permitted private environment using the instructions in package/README.md.

## Data and reproducibility boundaries

HRS input data require independent authorized access. Read [DATA_ACCESS.md](package/DATA_ACCESS.md). Do not upload HRS records, identifiers, predictions, fitted objects, private logs or participant-level figures to this repository or to AI tools.

The tested runtime is macOS with R 4.6.0. Dependency versions are listed in [DEPENDENCIES.tsv](package/environment/DEPENDENCIES.tsv). No dependency installation is automatic. The inventory is not a lockfile. Full Linux/Windows portability and installation on a fresh machine have not been demonstrated. A fresh checkout test verifies code portability in the available environment, not full scientific reproduction from private data.

Conditional ML intervals do not include full model-training uncertainty. No external validation, clinical utility or clinical equivalence is established. Tests cannot guarantee absolute error freedom. See [release gates](RELEASE_GATES.md).

## Version integrity

The package is intentionally a subdirectory: package verification checks every file and rejects extra files, so Git metadata stays outside it. Git attributes preserve byte-level hashes even when core.autocrlf is enabled. This is a file-integrity safeguard, not proof of Windows runtime support. Run Python with -B to avoid adding bytecode caches inside the package. Do not edit or store outputs inside package/ unless intentionally preparing a newly reviewed version and updating its manifests.

The authored code and documentation are provided under the [MIT license](LICENSE), Copyright (c) 2026 Steffen Held. This does not relicense HRS data or third-party packages. No separate code DOI is planned. The final release must identify the exact corrected code version used for the reported results.

## Code explanation and result checks

[Annotated source views](docs/README.md) explain the central statistical implementation with additional inline review comments. The executable source is preserved byte for byte because historical recovery uses exact source identities. The views are verified against those source bytes and are not alternative runnable versions.

[Corrected reported results](reported_results/README.md) and [independent arithmetic checks](audit/README.md) let reviewers check contrasts and multiplicity calculations without HRS records.

## AI-assisted development

OpenAI Codex (Astra 6) assisted with code development, documentation, and reproducibility checks, including detailed comments explaining analytical assumptions and implementation choices. The authors reviewed all AI-assisted outputs. Participant-level analyses were performed locally.

Reproducibility checks refer to the specific tests described here. They do not imply independent reconstruction of all manuscript results from private data or guarantee error-free code.

Release v1.0.1 adds the aggregate CSVs for Supplementary Tables S1, S5 and S6. [The supplementary CSV index](reported_results/README.md#supplementary-csv-completion-release-v101) locates all nine supplementary tables. Analytical source code and the v1.0.0 release are unchanged.
