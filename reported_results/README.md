# Corrected manuscript results, 5 October 2026

The original six aggregate CSV files are exact copies of the corrected manuscript table sources. They contain model-level estimates, intervals and conditional exploratory tests, not participant rows. SHA256.json records those six selected files.

Run `python3 -B audit/verify_results.py` from the repository root to check hashes, keyed model contrasts, primary paired differences, interval order, the complete 360-row Holm grid and full-training learning-curve point agreement. These checks establish internal agreement among released aggregates. They do not independently reproduce fits from private HRS inputs or prove confidence-interval coverage.

Table 2 is the primary nested optimism-corrected analysis. Table 3 is exploratory out-of-fold prediction. Table S2 uses conditional basic 90% intervals. S3 contains hypothetical margin tests, not proof of clinical equivalence. S4 uses 500-draw conditional learning-curve intervals, whereas Table 3 uses 5,000 draws. S8 is descriptive containment of primary 95% intervals. Do not interchange these interval types or analysis populations.

Historical fixtures inside package/ remain for exact replay guards. These corrected results are the current reporting reference. Figures and complete manuscript text are outside this code repository.

## Supplementary CSV completion, release v1.0.1

Three additional aggregate tables complete the nine supplementary CSVs available across this repository. These are unchanged copies of the current manuscript sources. No models were fitted and no analytical code changed in this release.

| Supplement table | CSV location relative to the repository root |
|---|---|
| S1, conditional associations | [reported_results/tableS1_associations.csv](tableS1_associations.csv) |
| S2, paired differences | [reported_results/tableS2_paired.csv](tableS2_paired.csv) |
| S3, exploratory margins | [reported_results/tableS3_equivalence.csv](tableS3_equivalence.csv) |
| S4, learning curves | [reported_results/tableS4_learning.csv](tableS4_learning.csv) |
| S5, simulations | [reported_results/tableS5_simulation.csv](tableS5_simulation.csv) |
| S6, augmentation associations | [reported_results/tableS6_augmentation_associations.csv](tableS6_augmentation_associations.csv) |
| S7, selected configurations | [package/reported_version/.../tableS7_tuning.csv](../package/reported_version/01_Aktuelles_Manuskript/manuskript_v2/tableS7_tuning.csv) |
| S8, interval containment | [reported_results/tableS8_original_margins.csv](tableS8_original_margins.csv) |
| S9, reconstructed fold counts | [package/corrected_version/overlay/.../fold_counts.csv](../package/corrected_version/overlay/01_Aktuelles_Manuskript/manuskript_v2/reporting_patch_20261004/fold_counts.csv) |

The three new files are covered by ADDITIONAL_SHA256SUMS. From this directory, run `shasum -a 256 -c ADDITIONAL_SHA256SUMS` on macOS or `sha256sum -c ADDITIONAL_SHA256SUMS` on Linux. The existing arithmetic verifier still covers its original six files. Hash verification of the added files establishes byte identity, not independent reproduction of the analyses.

S1 and S6 report different conditional association targets and bootstrap runs. Their contrasts and intervals must not be interpreted as causal effects. S5 reports variability across specified simulation mechanisms, not confidence intervals for a real-world effect or a test of clinical equivalence. See the manuscript supplement for definitions and limitations. GitHub availability does not itself establish compliance with a journal's long-term data-archiving requirements.
